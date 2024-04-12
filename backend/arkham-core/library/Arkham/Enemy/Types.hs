{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Enemy.Types (
  module Arkham.Enemy.Types,
  module X,
  Field (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack.Types
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Constants
import Arkham.Enemy.Cards
import Arkham.Enemy.Types.Attrs as X
import Arkham.GameValue
import Arkham.Id
import Arkham.Key
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Arkham.Token
import Arkham.Trait
import Control.Lens (non, over, set)
import Data.Data
import GHC.Records

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ EnemyId
  , EntityAttrs a ~ EnemyAttrs
  ) =>
  IsEnemy a

type EnemyCard a = CardBuilder EnemyId a

data instance Field Enemy :: Type -> Type where
  EnemyEngagedInvestigators :: Field Enemy (Set InvestigatorId)
  EnemyDoom :: Field Enemy Int
  EnemyEvade :: Field Enemy (Maybe Int)
  EnemyFight :: Field Enemy (Maybe Int)
  EnemyTokens :: Field Enemy Tokens
  EnemyClues :: Field Enemy Int
  EnemyDamage :: Field Enemy Int
  EnemyHealth :: Field Enemy (Maybe Int)
  EnemyHealthActual :: Field Enemy (Maybe GameValue)
  EnemyRemainingHealth :: Field Enemy (Maybe Int)
  EnemyForcedRemainingHealth :: Field Enemy Int
  EnemyHealthDamage :: Field Enemy Int
  EnemySanityDamage :: Field Enemy Int
  EnemyTraits :: Field Enemy (Set Trait)
  EnemyKeywords :: Field Enemy (Set Keyword)
  EnemyAbilities :: Field Enemy [Ability]
  EnemyCard :: Field Enemy Card
  EnemyName :: Field Enemy Name
  EnemyCardCode :: Field Enemy CardCode
  EnemyCardId :: Field Enemy CardId
  EnemyLocation :: Field Enemy (Maybe LocationId)
  EnemyPlacement :: Field Enemy Placement
  EnemySealedChaosTokens :: Field Enemy [ChaosToken]
  EnemyKeys :: Field Enemy (Set ArkhamKey)
  EnemySpawnedBy :: Field Enemy (Maybe InvestigatorId)
  EnemyAttacking :: Field Enemy (Maybe EnemyAttackDetails)

deriving stock instance Show (Field Enemy typ)
deriving stock instance Ord (Field Enemy typ)

instance ToJSON (Field Enemy typ) where
  toJSON = toJSON . show

instance Ord (SomeField Enemy) where
  compare (SomeField a) (SomeField b) = compare (show a) (show b)

instance Typeable typ => FromJSON (Field Enemy typ) where
  parseJSON x = do
    z <- parseJSON @(SomeField Enemy) x
    case z of
      SomeField (f :: Field Enemy k) -> case eqT @typ @k of
        Just Refl -> pure f
        Nothing -> error "type mismatch"

instance FromJSON (SomeField Enemy) where
  parseJSON = withText "Field Enemy" $ \case
    "EnemyEngagedInvestigators" -> pure $ SomeField EnemyEngagedInvestigators
    "EnemyDoom" -> pure $ SomeField EnemyDoom
    "EnemyEvade" -> pure $ SomeField Arkham.Enemy.Types.EnemyEvade
    "EnemyFight" -> pure $ SomeField Arkham.Enemy.Types.EnemyFight
    "EnemyTokens" -> pure $ SomeField EnemyTokens
    "EnemyClues" -> pure $ SomeField EnemyClues
    "EnemyDamage" -> pure $ SomeField EnemyDamage
    "EnemyHealth" -> pure $ SomeField EnemyHealth
    "EnemyHealthActual" -> pure $ SomeField EnemyHealthActual
    "EnemyRemainingHealth" -> pure $ SomeField EnemyRemainingHealth
    "EnemyForcedRemainingHealth" -> pure $ SomeField EnemyForcedRemainingHealth
    "EnemyHealthDamage" -> pure $ SomeField EnemyHealthDamage
    "EnemySanityDamage" -> pure $ SomeField EnemySanityDamage
    "EnemyTraits" -> pure $ SomeField EnemyTraits
    "EnemyKeywords" -> pure $ SomeField EnemyKeywords
    "EnemyAbilities" -> pure $ SomeField EnemyAbilities
    "EnemyCard" -> pure $ SomeField EnemyCard
    "EnemyName" -> pure $ SomeField EnemyName
    "EnemyCardCode" -> pure $ SomeField EnemyCardCode
    "EnemyCardId" -> pure $ SomeField EnemyCardId
    "EnemyLocation" -> pure $ SomeField EnemyLocation
    "EnemyPlacement" -> pure $ SomeField EnemyPlacement
    "EnemySealedChaosTokens" -> pure $ SomeField EnemySealedChaosTokens
    "EnemyKeys" -> pure $ SomeField EnemyKeys
    "EnemySpawnedBy" -> pure $ SomeField EnemySpawnedBy
    "EnemyAttacking" -> pure $ SomeField EnemyAttacking
    _ -> error "no such field"

data instance Field (OutOfPlayEntity Enemy) :: Type -> Type where
  OutOfPlayEnemyField :: OutOfPlayZone -> Field Enemy typ -> Field (OutOfPlayEntity Enemy) typ

allEnemyCards :: Map CardCode CardDef
allEnemyCards = allPlayerEnemyCards <> allEncounterEnemyCards <> allSpecialEnemyCards

instance IsCard EnemyAttrs where
  toCardId = enemyCardId
  toCard e = case lookupCard (enemyOriginalCardCode e) (toCardId e) of
    PlayerCard pc -> PlayerCard $ pc {pcOwner = enemyBearer e}
    ec -> ec
  toCardOwner = enemyBearer

instance HasCardDef EnemyAttrs where
  toCardDef e = case lookup (enemyCardCode e) allEnemyCards of
    Just def -> def
    Nothing -> error $ "missing card def for enemy " <> show (enemyCardCode e)

enemy
  :: (EnemyAttrs -> a)
  -> CardDef
  -> (Int, GameValue, Int)
  -> (Int, Int)
  -> CardBuilder EnemyId a
enemy f cardDef stats damageStats = enemyWith f cardDef stats damageStats id

enemyWith
  :: (EnemyAttrs -> a)
  -> CardDef
  -> (Int, GameValue, Int)
  -> (Int, Int)
  -> (EnemyAttrs -> EnemyAttrs)
  -> CardBuilder EnemyId a
enemyWith f cardDef (fight, health, evade) (healthDamage, sanityDamage) g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId eid ->
        f
          . g
          $ EnemyAttrs
            { enemyId = eid
            , enemyCardId = cardId
            , enemyCardCode = toCardCode cardDef
            , enemyOriginalCardCode = toCardCode cardDef
            , enemyPlacement = Unplaced
            , enemyFight = Just fight
            , enemyHealth = Just health
            , enemyEvade = Just evade
            , enemyAssignedDamage = mempty
            , enemyHealthDamage = healthDamage
            , enemySanityDamage = sanityDamage
            , enemyPrey = Prey Anyone
            , enemyModifiers = mempty
            , enemyExhausted = False
            , enemyTokens = mempty
            , enemySpawnAt = Nothing
            , enemySurgeIfUnableToSpawn = False
            , enemyAsSelfLocation = Nothing
            , enemyMovedFromHunterKeyword = False
            , enemyDamageStrategy = DamageAny
            , enemyBearer = Nothing
            , enemySealedChaosTokens = []
            , enemyKeys = mempty
            , enemySpawnedBy = Nothing
            , enemyDiscardedBy = Nothing
            , enemyDefeated = False
            , enemyAttacks = InvestigatorEngagedWith (EnemyWithId eid)
            , enemyUnableToSpawn = DiscardIfUnableToSpawn
            , enemyMeta = Null
            , enemyFlipped = False
            , enemyAttacking = Nothing
            }
    }

instance HasAbilities EnemyAttrs where
  getAbilities e =
    [ restrictedAbility
        e
        AbilityAttack
        ( OnSameLocation
            <> AnyCriterion
              [ Negate $ EnemyCriteria $ ThisEnemy AloofEnemy
              , EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith Anyone
              ]
            <> EnemyCriteria (ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
            <> CanAttack
        )
        $ ActionAbility [#fight] (ActionCost 1)
    , restrictedAbility
        e
        AbilityEvade
        (OnSameLocation <> EnemyCriteria (ThisEnemy $ EnemyIsEngagedWith You <> EnemyWithEvade))
        $ ActionAbility [#evade] (ActionCost 1)
    , restrictedAbility
        e
        AbilityEngage
        ( OnSameLocation
            <> Negate (EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith You)
            <> Negate (EnemyCriteria $ ThisEnemy MassiveEnemy)
            <> Negate (EnemyCriteria $ ThisEnemy $ EnemyWithPlacement Global)
            <> EnemyCriteria (ThisEnemy $ EnemyWithoutModifier CannotBeEngaged)
            <> InvestigatorExists (You <> InvestigatorWithoutModifier CannotBeEngaged)
        )
        $ ActionAbility [#engage] (ActionCost 1)
    ]

instance Entity EnemyAttrs where
  type EntityId EnemyAttrs = EnemyId
  type EntityAttrs EnemyAttrs = EnemyAttrs
  toId = enemyId
  toAttrs = id
  overAttrs f = f

instance Named EnemyAttrs where
  toName = toName . toCardDef

instance Targetable EnemyAttrs where
  toTarget = EnemyTarget . toId
  isTarget EnemyAttrs {enemyId} (EnemyTarget eid) = enemyId == eid
  isTarget attrs (CardCodeTarget cardCode) = toCardCode attrs == cardCode
  isTarget attrs (CardIdTarget cardId) = toCardId attrs == cardId
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance Sourceable EnemyAttrs where
  toSource = EnemySource . toId
  isSource EnemyAttrs {enemyId} (EnemySource eid) = enemyId == eid
  isSource attrs (CardCodeSource cardCode) = toCardCode attrs == cardCode
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource _ _ = False

instance HasField "ability" EnemyAttrs (Int -> Source) where
  getField = toAbilitySource

data Enemy = forall a. IsEnemy a => Enemy a

instance HasField "id" Enemy EnemyId where
  getField (Enemy e) = attr enemyId e

instance Data Enemy where
  gunfold _ _ _ = error "gunfold(Enemy)"
  toConstr _ = error "toConstr(Enemy)"
  dataTypeOf _ = error "dataTypeOf(Enemy)"

-- This instance might need to be defined in the future, I believe we'd need
-- something like the definition below, however this means that we'd need to
-- make every Enemy an instance of Data as well, which is a bit of a pain.
-- But if we need that simply add Data a to IsEnemy and follow the errors.

{-
data ExConstr = forall a. Typeable a => ExConstr a

instance Data Enemy where
  gunfold k z c | Just (ExConstr ec) <- cast c, Just (Enemy (_ :: a)) <- cast ec = k (z (Enemy :: a -> Enemy))
  gunfold _ _ c = error $ "gunfold(Enemy): " <> show c
  toConstr (Enemy _) = con_Enemy
  dataTypeOf _ = ty_Enemy

con_Enemy :: Constr
con_Enemy = mkConstr ty_Enemy "Enemy" [] Prefix

ty_Enemy :: DataType
ty_Enemy = mkDataType "Arkham.Enemy.Types.Enemy" [con_Enemy]
-}

instance Data (SomeField Enemy) where
  gunfold _ _ _ = error "gunfold(Enemy)"
  toConstr _ = error "toConstr(Enemy)"
  dataTypeOf _ = error "dataTypeOf(Enemy)"

instance Typeable a => Data (Field Enemy a) where
  gunfold _ _ _ = error "gunfold(Enemy)"
  toConstr _ = error "toConstr(Enemy)"
  dataTypeOf _ = error "dataTypeOf(Enemy)"

instance HasCardDef Enemy where
  toCardDef (Enemy a) = toCardDef (toAttrs a)
  {-# INLINE toCardDef #-}

instance HasCardCode Enemy where
  toCardCode (Enemy a) = toCardCode (toAttrs a)
  {-# INLINE toCardCode #-}

instance IsCard Enemy where
  toCard (Enemy a) = toCard (toAttrs a)
  {-# INLINE toCard #-}
  toCardId (Enemy a) = toCardId (toAttrs a)
  {-# INLINE toCardId #-}
  toCardOwner (Enemy a) = toCardOwner (toAttrs a)
  {-# INLINE toCardOwner #-}

instance Named Enemy where
  toName (Enemy a) = toName (toAttrs a)
  {-# INLINE toName #-}

instance Eq Enemy where
  Enemy (a :: a) == Enemy (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Enemy where
  show (Enemy a) = show a

instance ToJSON Enemy where
  toJSON (Enemy a) = toJSON a

instance HasAbilities Enemy where
  getAbilities (Enemy a) = getAbilities a

-- If the enemy has been defeated, we won't have removed it yet ourselves, but
-- the printed effects should be disabled
instance HasModifiersFor Enemy where
  getModifiersFor target (Enemy a) =
    if attr enemyDefeated a
      then pure mempty
      else getModifiersFor target a

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs (Enemy a) = toAttrs a
  overAttrs f (Enemy a) = Enemy $ overAttrs f a

instance Targetable Enemy where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

data SomeEnemyCard = forall a. IsEnemy a => SomeEnemyCard (EnemyCard a)

liftSomeEnemyCard :: (forall a. EnemyCard a -> b) -> SomeEnemyCard -> b
liftSomeEnemyCard f (SomeEnemyCard a) = f a

someEnemyCardCode :: SomeEnemyCard -> CardCode
someEnemyCardCode = liftSomeEnemyCard cbCardCode

makeLensesWith suffixedFields ''EnemyAttrs

fieldLens :: Field Enemy typ -> Lens' EnemyAttrs typ
fieldLens = \case
  EnemyEngagedInvestigators -> virtual
  EnemyDoom -> tokensL . at Doom . non 0
  Arkham.Enemy.Types.EnemyEvade -> evadeL
  Arkham.Enemy.Types.EnemyFight -> fightL
  EnemyTokens -> tokensL
  EnemyClues -> tokensL . at Clue . non 0
  EnemyDamage -> tokensL . at #damage . non 0
  EnemyHealthActual -> healthL
  EnemyHealth -> virtual
  EnemyRemainingHealth -> virtual
  EnemyForcedRemainingHealth -> virtual
  EnemyHealthDamage -> healthDamageL
  EnemySanityDamage -> sanityDamageL
  EnemyTraits -> virtual
  EnemyKeywords -> virtual
  EnemyAbilities -> virtual
  EnemyCard -> virtual
  EnemyName -> virtual
  EnemyCardCode -> cardCodeL
  EnemyCardId -> cardIdL
  EnemyLocation -> virtual
  EnemyPlacement -> placementL
  EnemySealedChaosTokens -> sealedChaosTokensL
  EnemyKeys -> keysL
  EnemySpawnedBy -> spawnedByL
  EnemyAttacking -> attackingL
 where
  virtual = error "virtual attribute can not be set directly"

updateEnemy :: [Update Enemy] -> EnemyAttrs -> EnemyAttrs
updateEnemy updates attrs = foldr go attrs updates
 where
  go :: Update Enemy -> EnemyAttrs -> EnemyAttrs
  go (Update fld val) = set (fieldLens fld) val
  go (IncrementBy fld val) = over (fieldLens fld) (max 0 . (+ val))
  go (DecrementBy fld val) = over (fieldLens fld) (max 0 . subtract val)

setMeta :: ToJSON a => a -> EnemyAttrs -> EnemyAttrs
setMeta a = metaL .~ toJSON a

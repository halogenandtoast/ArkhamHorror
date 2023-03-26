{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Enemy.Types
  ( module Arkham.Enemy.Types
  , module X
  , Field(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Constants
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards
import Arkham.Enemy.Types.Attrs as X
import Arkham.GameValue
import Arkham.Id
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
import Data.Typeable

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ EnemyId, EntityAttrs a ~ EnemyAttrs) => IsEnemy a

type EnemyCard a = CardBuilder EnemyId a

newtype VoidEnemy = VoidEnemy Enemy

data instance Field VoidEnemy :: Type -> Type where
  VoidEnemyCard :: Field VoidEnemy Card

data instance Field Enemy :: Type -> Type where
  EnemyEngagedInvestigators :: Field Enemy (HashSet InvestigatorId)
  EnemyDoom :: Field Enemy Int
  EnemyEvade :: Field Enemy (Maybe Int)
  EnemyFight :: Field Enemy Int
  EnemyClues :: Field Enemy Int
  EnemyDamage :: Field Enemy Int
  EnemyRemainingHealth :: Field Enemy Int
  EnemyHealthDamage :: Field Enemy Int
  EnemySanityDamage :: Field Enemy Int
  EnemyTraits :: Field Enemy (HashSet Trait)
  EnemyKeywords :: Field Enemy (HashSet Keyword)
  EnemyAbilities :: Field Enemy [Ability]
  EnemyCard :: Field Enemy Card
  EnemyCardCode :: Field Enemy CardCode
  EnemyCardId :: Field Enemy CardId
  EnemyLocation :: Field Enemy (Maybe LocationId)
  EnemyPlacement :: Field Enemy Placement
  EnemySealedTokens :: Field Enemy [Token]

data instance Field (SetAsideEntity Enemy) :: Type -> Type where
  SetAsideEnemyField :: Field Enemy typ -> Field (SetAsideEntity Enemy) typ

allEnemyCards :: HashMap CardCode CardDef
allEnemyCards = allPlayerEnemyCards <> allEncounterEnemyCards <> allSpecialEnemyCards

instance IsCard EnemyAttrs where
  toCardId = enemyCardId
  toCard e = case lookupCard (enemyOriginalCardCode e) (toCardId e) of
    PlayerCard pc -> PlayerCard $ pc { pcOwner = enemyBearer e }
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
    , cbCardBuilder = \cardId eid -> f . g $ EnemyAttrs
      { enemyId = eid
      , enemyCardId = cardId
      , enemyCardCode = toCardCode cardDef
      , enemyOriginalCardCode = toCardCode cardDef
      , enemyPlacement = Unplaced
      , enemyFight = fight
      , enemyHealth = health
      , enemyEvade = Just evade
      , enemyDamage = 0
      , enemyAssignedDamage = mempty
      , enemyHealthDamage = healthDamage
      , enemySanityDamage = sanityDamage
      , enemyPrey = Prey Anyone
      , enemyModifiers = mempty
      , enemyExhausted = False
      , enemyDoom = 0
      , enemyClues = 0
      , enemyResources = 0
      , enemySpawnAt = Nothing
      , enemySurgeIfUnableToSpawn = False
      , enemyAsSelfLocation = Nothing
      , enemyMovedFromHunterKeyword = False
      , enemyDamageStrategy = DamageAny
      , enemyBearer = Nothing
      , enemySealedTokens = []
      }
    }

instance HasAbilities EnemyAttrs where
  getAbilities e =
    [ restrictedAbility
        e
        AbilityAttack
        (OnSameLocation
        <> AnyCriterion
             [ Negate $ EnemyCriteria $ ThisEnemy AloofEnemy
             , EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith Anyone
             ]
        <> (EnemyCriteria $ ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
        )
      $ ActionAbility (Just Action.Fight) (ActionCost 1)
    , restrictedAbility
        e
        AbilityEvade
        (OnSameLocation <> EnemyCriteria (ThisEnemy $ EnemyIsEngagedWith You <> EnemyWithEvade))
      $ ActionAbility (Just Action.Evade) (ActionCost 1)
    , restrictedAbility
        e
        AbilityEngage
        (OnSameLocation
        <> Negate (EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith You)
        <> Negate (EnemyCriteria $ ThisEnemy MassiveEnemy)
        )
      $ ActionAbility (Just Action.Engage) (ActionCost 1)
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
  isTarget EnemyAttrs { enemyId } (EnemyTarget eid) = enemyId == eid
  isTarget attrs (CardCodeTarget cardCode) = toCardCode attrs == cardCode
  isTarget attrs (CardIdTarget cardId) = toCardId attrs == cardId
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance Sourceable EnemyAttrs where
  toSource = EnemySource . toId
  isSource EnemyAttrs { enemyId } (EnemySource eid) = enemyId == eid
  isSource attrs (CardCodeSource cardCode) = toCardCode attrs == cardCode
  isSource _ _ = False

data Enemy = forall a . IsEnemy a => Enemy a

type instance IdOf EnemyId = Enemy

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

instance HasModifiersFor Enemy where
  getModifiersFor target (Enemy a) = getModifiersFor target a

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs (Enemy a) = toAttrs a
  overAttrs f (Enemy a) = Enemy $ overAttrs f a

instance Entity VoidEnemy where
  type EntityId VoidEnemy = EnemyId
  type EntityAttrs VoidEnemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs (VoidEnemy (Enemy a)) = toAttrs a
  overAttrs f (VoidEnemy (Enemy a)) = VoidEnemy . Enemy $ overAttrs f a

instance Targetable Enemy where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

data SomeEnemyCard = forall a . IsEnemy a => SomeEnemyCard (EnemyCard a)

liftSomeEnemyCard :: (forall a . EnemyCard a -> b) -> SomeEnemyCard -> b
liftSomeEnemyCard f (SomeEnemyCard a) = f a

someEnemyCardCode :: SomeEnemyCard -> CardCode
someEnemyCardCode = liftSomeEnemyCard cbCardCode

makeLensesWith suffixedFields ''EnemyAttrs

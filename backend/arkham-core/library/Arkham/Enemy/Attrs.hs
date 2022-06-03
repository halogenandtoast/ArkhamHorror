module Arkham.Enemy.Attrs where

import Arkham.Prelude

import Arkham.Projection
import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.AssetId
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards
import Arkham.Enemy.Helpers
import Arkham.EnemyId
import Arkham.GameValue
import Arkham.InvestigatorId
import Arkham.Json
import Arkham.Keyword (HasKeywords(..), Keyword)
import Arkham.LocationId
import Arkham.Matcher
  ( EnemyMatcher(..)
  , PreyMatcher(..)
  , InvestigatorMatcher(Anyone, You)
  , LocationMatcher
  , pattern AloofEnemy
  )
import Arkham.Message
import Arkham.Modifier hiding (EnemyEvade)
import Arkham.Modifier qualified as Modifier
import Arkham.Name
import Arkham.Query
import Arkham.SkillTest
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.TreacheryId
import Arkham.Window qualified as Window

class IsEnemy a

type EnemyCard a = CardBuilder EnemyId a

data instance Field EnemyAttrs :: Type -> Type where
  EnemyDoom :: Field EnemyAttrs Int
  EnemyEvade :: Field EnemyAttrs Int
  EnemyHealthDamage :: Field EnemyAttrs EnemyHealthDamage

data EnemyAttrs = EnemyAttrs
  { enemyId :: EnemyId
  , enemyCardCode :: CardCode
  , enemyEngagedInvestigators :: HashSet InvestigatorId
  , enemyLocation :: Maybe LocationId
  , enemyFight :: Int
  , enemyHealth :: GameValue Int
  , enemyEvade :: Int
  , enemyDamage :: Int
  , enemyHealthDamage :: Int
  , enemySanityDamage :: Int
  , enemyTreacheries :: HashSet TreacheryId
  , enemyAssets :: HashSet AssetId
  , enemyPrey :: PreyMatcher
  , enemyModifiers :: HashMap Source [Modifier]
  , enemyExhausted :: Bool
  , enemyDoom :: Int
  , enemyClues :: Int
  , enemySpawnAt :: Maybe LocationMatcher
  , enemySurgeIfUnabledToSpawn :: Bool
  , enemyAsSelfLocation :: Maybe Text
  , enemyMovedFromHunterKeyword :: Bool
  , enemyDamageStrategy :: DamageStrategy
  , enemyBearer :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Generic)

damageStrategyL :: Lens' EnemyAttrs DamageStrategy
damageStrategyL =
  lens enemyDamageStrategy $ \m x -> m { enemyDamageStrategy = x }

movedFromHunterKeywordL :: Lens' EnemyAttrs Bool
movedFromHunterKeywordL = lens enemyMovedFromHunterKeyword
  $ \m x -> m { enemyMovedFromHunterKeyword = x }

bearerL :: Lens' EnemyAttrs (Maybe InvestigatorId)
bearerL = lens enemyBearer $ \m x -> m { enemyBearer = x }

spawnAtL :: Lens' EnemyAttrs (Maybe LocationMatcher)
spawnAtL = lens enemySpawnAt $ \m x -> m { enemySpawnAt = x }

surgeIfUnableToSpawnL :: Lens' EnemyAttrs Bool
surgeIfUnableToSpawnL = lens enemySurgeIfUnabledToSpawn $ \m x -> m { enemySurgeIfUnabledToSpawn = x }

healthDamageL :: Lens' EnemyAttrs Int
healthDamageL = lens enemyHealthDamage $ \m x -> m { enemyHealthDamage = x }

sanityDamageL :: Lens' EnemyAttrs Int
sanityDamageL = lens enemySanityDamage $ \m x -> m { enemySanityDamage = x }

healthL :: Lens' EnemyAttrs (GameValue Int)
healthL = lens enemyHealth $ \m x -> m { enemyHealth = x }

fightL :: Lens' EnemyAttrs Int
fightL = lens enemyFight $ \m x -> m { enemyFight = x }

evadeL :: Lens' EnemyAttrs Int
evadeL = lens enemyEvade $ \m x -> m { enemyEvade = x }

locationL :: Lens' EnemyAttrs (Maybe LocationId)
locationL = lens enemyLocation $ \m x -> m { enemyLocation = x }

asSelfLocationL :: Lens' EnemyAttrs (Maybe Text)
asSelfLocationL =
  lens enemyAsSelfLocation $ \m x -> m { enemyAsSelfLocation = x }

preyL :: Lens' EnemyAttrs PreyMatcher
preyL = lens enemyPrey $ \m x -> m { enemyPrey = x }

treacheriesL :: Lens' EnemyAttrs (HashSet TreacheryId)
treacheriesL = lens enemyTreacheries $ \m x -> m { enemyTreacheries = x }

assetsL :: Lens' EnemyAttrs (HashSet AssetId)
assetsL = lens enemyAssets $ \m x -> m { enemyAssets = x }

damageL :: Lens' EnemyAttrs Int
damageL = lens enemyDamage $ \m x -> m { enemyDamage = x }

engagedInvestigatorsL :: Lens' EnemyAttrs (HashSet InvestigatorId)
engagedInvestigatorsL =
  lens enemyEngagedInvestigators $ \m x -> m { enemyEngagedInvestigators = x }

exhaustedL :: Lens' EnemyAttrs Bool
exhaustedL = lens enemyExhausted $ \m x -> m { enemyExhausted = x }

doomL :: Lens' EnemyAttrs Int
doomL = lens enemyDoom $ \m x -> m { enemyDoom = x }

cluesL :: Lens' EnemyAttrs Int
cluesL = lens enemyClues $ \m x -> m { enemyClues = x }

allEnemyCards :: HashMap CardCode CardDef
allEnemyCards = allPlayerEnemyCards <> allEncounterEnemyCards

instance HasName env EnemyAttrs where
  getName = pure . toName

instance HasCardCode EnemyAttrs where
  toCardCode = enemyCardCode

instance HasCardDef EnemyAttrs where
  toCardDef e = case lookup (enemyCardCode e) allEnemyCards of
    Just def -> def
    Nothing -> error $ "missing card def for enemy " <> show (enemyCardCode e)

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs { enemyLocation } = enemyLocation /= Nothing

instance ToJSON EnemyAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "enemy"
  toEncoding = genericToEncoding $ aesonOptions $ Just "enemy"

instance FromJSON EnemyAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "enemy"

instance IsCard EnemyAttrs where
  toCardId = unEnemyId . enemyId
  toCardOwner = enemyBearer

enemy
  :: (EnemyAttrs -> a)
  -> CardDef
  -> (Int, GameValue Int, Int)
  -> (Int, Int)
  -> CardBuilder EnemyId a
enemy f cardDef stats damageStats = enemyWith f cardDef stats damageStats id

enemyWith
  :: (EnemyAttrs -> a)
  -> CardDef
  -> (Int, GameValue Int, Int)
  -> (Int, Int)
  -> (EnemyAttrs -> EnemyAttrs)
  -> CardBuilder EnemyId a
enemyWith f cardDef (fight, health, evade) (healthDamage, sanityDamage) g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \eid -> f . g $ EnemyAttrs
      { enemyId = eid
      , enemyCardCode = toCardCode cardDef
      , enemyEngagedInvestigators = mempty
      , enemyLocation = Nothing
      , enemyFight = fight
      , enemyHealth = health
      , enemyEvade = evade
      , enemyDamage = 0
      , enemyHealthDamage = healthDamage
      , enemySanityDamage = sanityDamage
      , enemyTreacheries = mempty
      , enemyAssets = mempty
      , enemyPrey = Prey Anyone
      , enemyModifiers = mempty
      , enemyExhausted = False
      , enemyDoom = 0
      , enemyClues = 0
      , enemySpawnAt = Nothing
      , enemySurgeIfUnabledToSpawn = False
      , enemyAsSelfLocation = Nothing
      , enemyMovedFromHunterKeyword = False
      , enemyDamageStrategy = DamageAny
      , enemyBearer = Nothing
      }
    }

spawnAt
  :: (MonadIO m, MonadReader env m, HasQueue env, HasSet InvestigatorId env ())
  => EnemyId
  -> LocationMatcher
  -> m ()
spawnAt eid locationMatcher = do
  windows' <- windows [Window.EnemyAttemptsToSpawnAt eid locationMatcher]
  pushAll $ windows' <> resolve
    (EnemySpawnAtLocationMatching Nothing locationMatcher eid)

modifiedEnemyFight
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> m Int
modifiedEnemyFight EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier enemyFight modifiers'
 where
  applyModifier (EnemyFight m) n = max 0 (n + m)
  applyModifier _ n = n

modifiedEnemyEvade
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> m Int
modifiedEnemyEvade EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier enemyEvade modifiers'
 where
  applyModifier (Modifier.EnemyEvade m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedDamageAmount
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> Bool
  -> Int
  -> m Int
getModifiedDamageAmount EnemyAttrs {..} direct baseAmount = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  let updatedAmount = foldr applyModifier baseAmount modifiers'
  pure $ foldr applyModifierCaps updatedAmount modifiers'
 where
  applyModifier (DamageTaken m) n | not direct = max 0 (n + m)
  applyModifier _ n = n
  applyModifierCaps (MaxDamageTaken m) n = min m n
  applyModifierCaps _ n = n

getModifiedKeywords
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> m (HashSet Keyword)
getModifiedKeywords e@EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier (toKeywords $ toCardDef e) modifiers'
 where
  applyModifier (AddKeyword k) n = insertSet k n
  applyModifier _ n = n

canEnterLocation
  :: (HasModifiersFor env (), HasSet Trait env EnemyId, MonadReader env m)
  => EnemyId
  -> LocationId
  -> m Bool
canEnterLocation eid lid = do
  traits <- getSet eid
  modifiers' <- getModifiers (EnemySource eid) (LocationTarget lid)
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits
    _ -> False

instance HasAbilities EnemyAttrs where
  getAbilities e =
    [ restrictedAbility
        e
        100
        (OnSameLocation <> AnyCriterion
          [ Negate $ EnemyCriteria $ ThisEnemy AloofEnemy
          , EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith Anyone
          ]
        )
      $ ActionAbility (Just Action.Fight) (ActionCost 1)
    , restrictedAbility
        e
        101
        (OnSameLocation <> EnemyCriteria (ThisEnemy $ EnemyIsEngagedWith You))
      $ ActionAbility (Just Action.Evade) (ActionCost 1)
    , restrictedAbility
        e
        102
        (OnSameLocation
        <> Negate (EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith You)
        )
      $ ActionAbility (Just Action.Engage) (ActionCost 1)
    ]

instance Entity EnemyAttrs where
  type EntityId EnemyAttrs = EnemyId
  type EntityAttrs EnemyAttrs = EnemyAttrs
  toId = enemyId
  toAttrs = id

instance Named EnemyAttrs where
  toName = toName . toCardDef

instance TargetEntity EnemyAttrs where
  toTarget = EnemyTarget . toId
  isTarget EnemyAttrs { enemyId } (EnemyTarget eid) = enemyId == eid
  isTarget attrs (CardCodeTarget cardCode) = toCardCode attrs == cardCode
  isTarget attrs (CardIdTarget cardId) = toCardId attrs == cardId
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity EnemyAttrs where
  toSource = EnemySource . toId
  isSource EnemyAttrs { enemyId } (EnemySource eid) = enemyId == eid
  isSource attrs (CardCodeSource cardCode) = toCardCode attrs == cardCode
  isSource _ _ = False

getModifiedHealth
  :: (MonadReader env m, HasModifiersFor env (), HasCount PlayerCount env ())
  => EnemyAttrs
  -> m Int
getModifiedHealth EnemyAttrs {..} = do
  playerCount <- getPlayerCount
  modifiers' <- getModifiers (EnemySource enemyId) (EnemyTarget enemyId)
  pure $ foldr applyModifier (fromGameValue enemyHealth playerCount) modifiers'
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

emptyLocationMap :: HashMap LocationId [LocationId]
emptyLocationMap = mempty

isActionTarget :: EnemyAttrs -> Target -> Bool
isActionTarget attrs = isTarget attrs . toProxyTarget

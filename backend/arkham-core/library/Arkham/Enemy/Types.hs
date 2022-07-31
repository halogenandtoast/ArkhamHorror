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

data instance Field Enemy :: Type -> Type where
  EnemyEngagedInvestigators :: Field Enemy (HashSet InvestigatorId)
  EnemyDoom :: Field Enemy Int
  EnemyEvade :: Field Enemy Int
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
  EnemyLocation :: Field Enemy (Maybe LocationId)
  EnemyPlacement :: Field Enemy Placement
  EnemySealedTokens :: Field Enemy [Token]

sealedTokensL :: Lens' EnemyAttrs [Token]
sealedTokensL = lens enemySealedTokens $ \m x -> m { enemySealedTokens = x }

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
surgeIfUnableToSpawnL =
  lens enemySurgeIfUnabledToSpawn $ \m x -> m { enemySurgeIfUnabledToSpawn = x }

placementL :: Lens' EnemyAttrs Placement
placementL = lens enemyPlacement $ \m x -> m { enemyPlacement = x }

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

asSelfLocationL :: Lens' EnemyAttrs (Maybe Text)
asSelfLocationL =
  lens enemyAsSelfLocation $ \m x -> m { enemyAsSelfLocation = x }

preyL :: Lens' EnemyAttrs PreyMatcher
preyL = lens enemyPrey $ \m x -> m { enemyPrey = x }

damageL :: Lens' EnemyAttrs Int
damageL = lens enemyDamage $ \m x -> m { enemyDamage = x }

exhaustedL :: Lens' EnemyAttrs Bool
exhaustedL = lens enemyExhausted $ \m x -> m { enemyExhausted = x }

doomL :: Lens' EnemyAttrs Int
doomL = lens enemyDoom $ \m x -> m { enemyDoom = x }

cluesL :: Lens' EnemyAttrs Int
cluesL = lens enemyClues $ \m x -> m { enemyClues = x }

allEnemyCards :: HashMap CardCode CardDef
allEnemyCards = allPlayerEnemyCards <> allEncounterEnemyCards

instance IsCard EnemyAttrs where
  toCardId = unEnemyId . enemyId
  toCardOwner = enemyBearer

instance HasCardDef EnemyAttrs where
  toCardDef e = case lookup (enemyCardCode e) allEnemyCards of
    Just def -> def
    Nothing -> error $ "missing card def for enemy " <> show (enemyCardCode e)

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
      , enemyPlacement = Unplaced
      , enemyFight = fight
      , enemyHealth = health
      , enemyEvade = evade
      , enemyDamage = 0
      , enemyHealthDamage = healthDamage
      , enemySanityDamage = sanityDamage
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
      , enemySealedTokens = []
      }
    }

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
  overAttrs f = f

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

data Enemy = forall a . IsEnemy a => Enemy a

instance Eq Enemy where
  (Enemy (a :: a)) == (Enemy (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Enemy where
  show (Enemy a) = show a

instance ToJSON Enemy where
  toJSON (Enemy a) = toJSON a

instance HasAbilities Enemy where
  getAbilities (Enemy a) = getAbilities a

instance HasModifiersFor Enemy where
  getModifiersFor source target (Enemy a) = getModifiersFor source target a

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs (Enemy a) = toAttrs a
  overAttrs f (Enemy a) = Enemy $ overAttrs f a

instance TargetEntity Enemy where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

data SomeEnemyCard = forall a . IsEnemy a => SomeEnemyCard (EnemyCard a)

liftSomeEnemyCard :: (forall a . EnemyCard a -> b) -> SomeEnemyCard -> b
liftSomeEnemyCard f (SomeEnemyCard a) = f a

someEnemyCardCode :: SomeEnemyCard -> CardCode
someEnemyCardCode = liftSomeEnemyCard cbCardCode


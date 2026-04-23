module Arkham.EnemyLocation.EnemyProxy (toEnemyLocationEnemyProxy) where

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities (HasAbilities (..))
import Arkham.Classes.HasModifiersFor (HasModifiersFor (..))
import Arkham.Classes.RunMessage.Internal (RunMessage (..))
import Arkham.Constants
import Arkham.Enemy.Types (
  Enemy (..),
  EnemyAttrs (..),
  IsEnemy,
  UnableToSpawn (DiscardIfUnableToSpawn),
 )
import Arkham.EnemyLocation.Cards (allEnemyLocationCards)
import Arkham.EnemyLocation.Types
import Arkham.Id
import Arkham.Matcher (InvestigatorMatcher (Anyone), PreyMatcher (..))
import Arkham.Placement (Placement (AtLocation))
import Arkham.Prelude
import Arkham.Strategy (DamageStrategy (DamageAny))

newtype EnemyLocationEnemyProxy = EnemyLocationEnemyProxy EnemyLocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasCardCode EnemyLocationEnemyProxy where
  toCardCode (EnemyLocationEnemyProxy a) = enemyLocationCardCode a

instance HasCardDef EnemyLocationEnemyProxy where
  toCardDef (EnemyLocationEnemyProxy a) =
    case lookup (enemyLocationCardCode a) allEnemyLocationCards of
      Just def -> def
      Nothing -> error $ "missing card def for enemy-location enemy proxy " <> show (enemyLocationCardCode a)

instance HasAbilities EnemyLocationEnemyProxy where
  -- Expose fight and evade abilities so CanFightEnemy/CanEvadeEnemy matchers
  -- can find this proxy. OnSameLocation captures the Massive rule.
  getAbilities (EnemyLocationEnemyProxy a) =
    [ basicAbility
        $ restricted a AbilityAttack (OnSameLocation <> CanAttack)
        $ ActionAbility #fight #combat (ActionCost 1)
    , basicAbility
        $ restricted a AbilityEvade OnSameLocation
        $ ActionAbility #evade #agility (ActionCost 1)
    ]

instance HasModifiersFor EnemyLocationEnemyProxy where
  getModifiersFor _ = pure ()

instance RunMessage EnemyLocationEnemyProxy where
  runMessage _ p = pure p

instance IsEnemy EnemyLocationEnemyProxy

instance Entity EnemyLocationEnemyProxy where
  type EntityId EnemyLocationEnemyProxy = EnemyId
  type EntityAttrs EnemyLocationEnemyProxy = EnemyAttrs
  toId (EnemyLocationEnemyProxy a) = enemyLocationAsEnemyId (EnemyLocationId (enemyLocationId a))
  toAttrs (EnemyLocationEnemyProxy a) = toProxyEnemyAttrs a
  overAttrs _ p = p

toProxyEnemyAttrs :: EnemyLocationAttrs -> EnemyAttrs
toProxyEnemyAttrs ela =
  EnemyAttrs
    { enemyId = enemyLocationAsEnemyId (EnemyLocationId (enemyLocationId ela))
    , enemyCardId = enemyLocationCardId ela
    , enemyCardCode = enemyLocationCardCode ela
    , enemyOriginalCardCode = enemyLocationOriginalCardCode ela
    , enemyPlacement = AtLocation (enemyLocationId ela)
    , enemyFight = enemyLocationFight ela
    , enemyHealth = enemyLocationHealth ela
    , enemyEvade = enemyLocationEvade ela
    , enemyAssignedDamage = mempty
    , enemyHealthDamage = enemyLocationHealthDamage ela
    , enemySanityDamage = enemyLocationSanityDamage ela
    , enemyPrey = Prey Anyone
    , enemyModifiers = mempty
    , enemyExhausted = enemyLocationExhausted ela
    , enemyTokens = enemyLocationTokens ela
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
    , enemyDefeated = enemyLocationDefeated ela
    , enemyAttacks = Anyone
    , enemyUnableToSpawn = DiscardIfUnableToSpawn
    , enemyMeta = Null
    , enemyFlipped = False
    , enemyWantsToAttack = False
    , enemyAttacking = Nothing
    , enemyDelayEngagement = False
    , enemyCardsUnderneath = []
    , enemyLastKnownLocation = Nothing
    , enemyReferenceCards = []
    , enemySpawnDetails = Nothing
    , enemyMovement = Nothing
    , enemyAttackOfOpportunityFlagged = False
    , enemyDrawnFrom = Nothing
    }

toEnemyLocationEnemyProxy :: EnemyLocationAttrs -> Enemy
toEnemyLocationEnemyProxy = Enemy . EnemyLocationEnemyProxy

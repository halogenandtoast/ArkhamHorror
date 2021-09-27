module Arkham.Types.Enemy.Cards.TheRougarou
  ( TheRougarou(..)
  , theRougarou
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype TheRougarouMetadata = TheRougarouMetadata { damagePerPhase :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheRougarou = TheRougarou (EnemyAttrs `With` TheRougarouMetadata)
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRougarou :: EnemyCard TheRougarou
theRougarou = enemy
  (TheRougarou . (`with` TheRougarouMetadata 0))
  Cards.theRougarou
  (3, PerPlayer 5, 3)
  (2, 2)

isEngage :: Ability -> Bool
isEngage ability = case abilityType ability of
  ActionAbility (Just Action.Engage) _ -> True
  _ -> False

instance HasAbilities TheRougarou where
  getAbilities (TheRougarou (attrs `With` meta)) = do
    let
      actions' = getAbilities attrs
      forcedAbility =
        restrictedAbility
            attrs
            1
            (ValueIs (damagePerPhase meta) (EqualTo $ PerPlayer 1))
            (ForcedAbility $ EnemyDealtDamage
              Timing.After
              AnyDamageEffect
              (EnemyWithId $ toId attrs)
              AnySource
            )
          & (abilityLimitL .~ NoLimit)
    if any isEngage actions'
      then do
        let
          engageAction =
            mkAbility attrs 102
              $ ActionAbility (Just Action.Engage)
              $ GroupClueCost (ByPlayerCount 1 1 2 2) Anywhere
              <> ActionCost 1
        forcedAbility : filter (not . isEngage) actions' <> [engageAction]
      else forcedAbility : actions'

instance EnemyRunner env => RunMessage env TheRougarou where
  runMessage msg (TheRougarou (attrs@EnemyAttrs {..} `With` metadata)) =
    case msg of
      UseCardAbility _ source _ 1 _ | isSource attrs source -> do
        damageThreshold <- getPlayerCountValue (PerPlayer 1)
        investigatorIds <- getInvestigatorIds
        leadInvestigatorId <- getLeadInvestigatorId
        farthestLocationIds <- case investigatorIds of
          [iid] -> map unFarthestLocationId <$> getSetList iid
          iids -> map unFarthestLocationId <$> getSetList iids
        case farthestLocationIds of
          [] -> error "can't happen"
          [x] -> push (MoveUntil x (EnemyTarget enemyId))
          xs -> push
            (chooseOne
              leadInvestigatorId
              [ MoveUntil x (EnemyTarget enemyId) | x <- xs ]
            )

        TheRougarou
          . (`with` TheRougarouMetadata
              (damagePerPhase metadata `mod` damageThreshold)
            )
          <$> runMessage msg attrs
      EndPhase ->
        TheRougarou . (`with` TheRougarouMetadata 0) <$> runMessage msg attrs
      EnemyDamage eid _ _ _ n | eid == enemyId ->
        TheRougarou
          . (`with` TheRougarouMetadata (damagePerPhase metadata + n))
          <$> runMessage msg attrs
      _ -> TheRougarou . (`with` metadata) <$> runMessage msg attrs

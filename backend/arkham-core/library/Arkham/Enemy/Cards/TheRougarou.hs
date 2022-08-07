module Arkham.Enemy.Cards.TheRougarou
  ( TheRougarou(..)
  , theRougarou
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype TheRougarouMetadata = TheRougarouMetadata { damagePerPhase :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheRougarou = TheRougarou (EnemyAttrs `With` TheRougarouMetadata)
  deriving anyclass (IsEnemy, HasModifiersFor)
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
            restrictedAbility
                attrs
                102
                (OnSameLocation <> Negate
                  (EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith You)
                )
              $ ActionAbility (Just Action.Engage)
              $ GroupClueCost (ByPlayerCount 1 1 2 2) Anywhere
              <> ActionCost 1
        forcedAbility : filter (not . isEngage) actions' <> [engageAction]
      else forcedAbility : actions'

instance RunMessage TheRougarou where
  runMessage msg (TheRougarou (attrs@EnemyAttrs {..} `With` metadata)) =
    case msg of
      UseCardAbility _ source _ 1 _ | isSource attrs source -> do
        damageThreshold <- getPlayerCountValue (PerPlayer 1)
        leadInvestigatorId <- getLeadInvestigatorId
        farthestLocationIds <- selectList $ FarthestLocationFromAll Anywhere
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
      Msg.EnemyDamage eid _ _ _ n | eid == enemyId ->
        TheRougarou
          . (`with` TheRougarouMetadata (damagePerPhase metadata + n))
          <$> runMessage msg attrs
      _ -> TheRougarou . (`with` metadata) <$> runMessage msg attrs

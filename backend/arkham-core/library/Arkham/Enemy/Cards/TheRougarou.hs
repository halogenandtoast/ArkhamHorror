module Arkham.Enemy.Cards.TheRougarou (
  TheRougarou (..),
  theRougarou,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype TheRougarouMetadata = TheRougarouMetadata {damagePerPhase :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks)

newtype TheRougarou = TheRougarou (EnemyAttrs `With` TheRougarouMetadata)
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theRougarou :: EnemyCard TheRougarou
theRougarou =
  enemy
    (TheRougarou . (`with` TheRougarouMetadata 0))
    Cards.theRougarou
    (3, PerPlayer 5, 3)
    (2, 2)

isEngage :: Ability -> Bool
isEngage ability = case abilityType ability of
  ActionAbility actions _ -> Action.Engage `elem` actions
  _ -> False

instance HasAbilities TheRougarou where
  getAbilities (TheRougarou (attrs `With` meta)) = do
    let
      actions' = getAbilities attrs
      firstAbility =
        restrictedAbility
          attrs
          1
          (ValueIs (damagePerPhase meta) (EqualTo $ PerPlayer 1))
          ( ForcedAbility
              $ EnemyDealtDamage
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
              ( OnSameLocation
                  <> Negate
                    (EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith You)
              )
              $ ActionAbility [Action.Engage]
              $ GroupClueCost (ByPlayerCount 1 1 2 2) Anywhere
              <> ActionCost 1
        firstAbility : filter (not . isEngage) actions' <> [engageAction]
      else firstAbility : actions'

instance RunMessage TheRougarou where
  runMessage msg (TheRougarou (attrs@EnemyAttrs {..} `With` metadata)) =
    case msg of
      UseCardAbility _ source 1 _ _ | isSource attrs source -> do
        damageThreshold <- getPlayerCountValue (PerPlayer 1)
        lead <- getLeadPlayer
        farthestLocationIds <- selectList $ FarthestLocationFromAll Anywhere
        case farthestLocationIds of
          [] -> error "can't happen"
          [x] -> push (MoveUntil x (EnemyTarget enemyId))
          xs -> push $ chooseOne lead [targetLabel x [MoveUntil x (EnemyTarget enemyId)] | x <- xs]

        TheRougarou
          . ( `with`
                TheRougarouMetadata
                  (damagePerPhase metadata `mod` damageThreshold)
            )
          <$> runMessage msg attrs
      EndPhase -> do
        TheRougarou . (`with` TheRougarouMetadata 0) <$> runMessage msg attrs
      Msg.EnemyDamage eid (damageAssignmentAmount -> n) | eid == enemyId -> do
        TheRougarou
          . (`with` TheRougarouMetadata (damagePerPhase metadata + n))
          <$> runMessage msg attrs
      _ -> TheRougarou . (`with` metadata) <$> runMessage msg attrs

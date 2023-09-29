module Arkham.Act.Cards.TheReturnTrip (
  TheReturnTrip (..),
  theReturnTrip,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Ability
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype TheReturnTrip = TheReturnTrip ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theReturnTrip :: ActCard TheReturnTrip
theReturnTrip =
  act
    (3, A)
    TheReturnTrip
    Cards.theReturnTrip
    (Just $ GroupClueCost (PerPlayer 2) $ LocationWithTitle "Templo Mayor")

instance HasAbilities TheReturnTrip where
  getAbilities (TheReturnTrip a) =
    withBaseAbilities
      a
      [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ EnemyDefeated Timing.When Anyone ByAny
        $ enemyIs Enemies.padmaAmrita
      | onSide A a
      ]

instance RunMessage TheReturnTrip where
  runMessage msg a@(TheReturnTrip attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    _ -> TheReturnTrip <$> runMessage msg attrs

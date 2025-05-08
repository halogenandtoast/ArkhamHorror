module Arkham.Location.Cards.TearThroughTime (tearThroughTime) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.LostInTimeAndSpace.Helpers

newtype TearThroughTime = TearThroughTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughTime :: LocationCard TearThroughTime
tearThroughTime = location TearThroughTime Cards.tearThroughTime 2 (PerPlayer 2)

instance HasAbilities TearThroughTime where
  getAbilities (TearThroughTime a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "tearThroughTime.resign"
      $ mkAbility a 99
      $ ActionAbility [#resign] (ActionCost 1 <> ClueCost (Static 2))

instance RunMessage TearThroughTime where
  runMessage msg (TearThroughTime attrs) =
    TearThroughTime <$> runMessage msg attrs

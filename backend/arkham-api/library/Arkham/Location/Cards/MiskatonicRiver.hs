module Arkham.Location.Cards.MiskatonicRiver (miskatonicRiver) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype MiskatonicRiver = MiskatonicRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicRiver :: LocationCard MiskatonicRiver
miskatonicRiver = location MiskatonicRiver Cards.miskatonicRiver 5 (Static 0)

instance HasAbilities MiskatonicRiver where
  getAbilities (MiskatonicRiver a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "miskatonicRiver.resign" (locationResignAction a)

instance RunMessage MiskatonicRiver where
  runMessage msg (MiskatonicRiver attrs) =
    MiskatonicRiver <$> runMessage msg attrs

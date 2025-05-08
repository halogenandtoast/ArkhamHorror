module Arkham.Location.Cards.VillageCommons (villageCommons) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (villageCommons)
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.BloodOnTheAltar.Helpers

newtype VillageCommons = VillageCommons LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

villageCommons :: LocationCard VillageCommons
villageCommons = location VillageCommons Cards.villageCommons 3 (Static 0)

instance HasAbilities VillageCommons where
  getAbilities (VillageCommons a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "villageCommons.resign" $ locationResignAction a

instance RunMessage VillageCommons where
  runMessage msg (VillageCommons attrs) =
    VillageCommons <$> runMessage msg attrs

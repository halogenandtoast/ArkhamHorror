module Arkham.Location.Cards.SandsOfDashur (sandsOfDashur) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SandsOfDashur = SandsOfDashur LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sandsOfDashur :: LocationCard SandsOfDashur
sandsOfDashur = location SandsOfDashur Cards.sandsOfDashur 7 (Static 0)

instance HasModifiersFor SandsOfDashur where
  getModifiersFor (SandsOfDashur a) = do
    modifySelf a [AdditionalCostToExplore (ResourceCost 2)]

instance RunMessage SandsOfDashur where
  runMessage msg (SandsOfDashur attrs) = SandsOfDashur <$> runMessage msg attrs

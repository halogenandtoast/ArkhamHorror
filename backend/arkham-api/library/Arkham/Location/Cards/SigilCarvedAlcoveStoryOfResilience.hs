module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfResilience (sigilCarvedAlcoveStoryOfResilience) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SigilCarvedAlcoveStoryOfResilience = SigilCarvedAlcoveStoryOfResilience LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfResilience :: LocationCard SigilCarvedAlcoveStoryOfResilience
sigilCarvedAlcoveStoryOfResilience = location SigilCarvedAlcoveStoryOfResilience Cards.sigilCarvedAlcoveStoryOfResilience 4 (Static 1)

-- TODO: abilities

instance RunMessage SigilCarvedAlcoveStoryOfResilience where
  runMessage msg (SigilCarvedAlcoveStoryOfResilience attrs) = runQueueT $ SigilCarvedAlcoveStoryOfResilience <$> liftRunMessage msg attrs

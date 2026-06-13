module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfAmbition (sigilCarvedAlcoveStoryOfAmbition) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SigilCarvedAlcoveStoryOfAmbition = SigilCarvedAlcoveStoryOfAmbition LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfAmbition :: LocationCard SigilCarvedAlcoveStoryOfAmbition
sigilCarvedAlcoveStoryOfAmbition = location SigilCarvedAlcoveStoryOfAmbition Cards.sigilCarvedAlcoveStoryOfAmbition 4 (Static 1)

-- TODO: abilities

instance RunMessage SigilCarvedAlcoveStoryOfAmbition where
  runMessage msg (SigilCarvedAlcoveStoryOfAmbition attrs) = runQueueT $ SigilCarvedAlcoveStoryOfAmbition <$> liftRunMessage msg attrs

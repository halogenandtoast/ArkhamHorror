module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfDefiance (sigilCarvedAlcoveStoryOfDefiance) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SigilCarvedAlcoveStoryOfDefiance = SigilCarvedAlcoveStoryOfDefiance LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfDefiance :: LocationCard SigilCarvedAlcoveStoryOfDefiance
sigilCarvedAlcoveStoryOfDefiance = location SigilCarvedAlcoveStoryOfDefiance Cards.sigilCarvedAlcoveStoryOfDefiance 4 (Static 1)

-- TODO: abilities

instance RunMessage SigilCarvedAlcoveStoryOfDefiance where
  runMessage msg (SigilCarvedAlcoveStoryOfDefiance attrs) = runQueueT $ SigilCarvedAlcoveStoryOfDefiance <$> liftRunMessage msg attrs

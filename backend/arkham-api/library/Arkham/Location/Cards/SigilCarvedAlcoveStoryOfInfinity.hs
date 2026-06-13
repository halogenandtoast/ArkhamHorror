module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfInfinity (sigilCarvedAlcoveStoryOfInfinity) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SigilCarvedAlcoveStoryOfInfinity = SigilCarvedAlcoveStoryOfInfinity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfInfinity :: LocationCard SigilCarvedAlcoveStoryOfInfinity
sigilCarvedAlcoveStoryOfInfinity = location SigilCarvedAlcoveStoryOfInfinity Cards.sigilCarvedAlcoveStoryOfInfinity 4 (Static 1)

-- TODO: abilities

instance RunMessage SigilCarvedAlcoveStoryOfInfinity where
  runMessage msg (SigilCarvedAlcoveStoryOfInfinity attrs) = runQueueT $ SigilCarvedAlcoveStoryOfInfinity <$> liftRunMessage msg attrs

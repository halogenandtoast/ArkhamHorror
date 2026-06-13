module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfTheVoyage (sigilCarvedAlcoveStoryOfTheVoyage) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SigilCarvedAlcoveStoryOfTheVoyage = SigilCarvedAlcoveStoryOfTheVoyage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfTheVoyage :: LocationCard SigilCarvedAlcoveStoryOfTheVoyage
sigilCarvedAlcoveStoryOfTheVoyage = location SigilCarvedAlcoveStoryOfTheVoyage Cards.sigilCarvedAlcoveStoryOfTheVoyage 4 (Static 1)

-- TODO: abilities

instance RunMessage SigilCarvedAlcoveStoryOfTheVoyage where
  runMessage msg (SigilCarvedAlcoveStoryOfTheVoyage attrs) = runQueueT $ SigilCarvedAlcoveStoryOfTheVoyage <$> liftRunMessage msg attrs

module Arkham.Location.Cards.ReturnToGardensOfLuxembourg (returnToGardensOfLuxembourg) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToGardensOfLuxembourg = ReturnToGardensOfLuxembourg LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToGardensOfLuxembourg :: LocationCard ReturnToGardensOfLuxembourg
returnToGardensOfLuxembourg = location ReturnToGardensOfLuxembourg Cards.returnToGardensOfLuxembourg 2 (PerPlayer 1)

instance HasAbilities ReturnToGardensOfLuxembourg where
  getAbilities (ReturnToGardensOfLuxembourg attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToGardensOfLuxembourg where
  runMessage msg (ReturnToGardensOfLuxembourg attrs) = runQueueT $ case msg of
    _ -> ReturnToGardensOfLuxembourg <$> liftRunMessage msg attrs

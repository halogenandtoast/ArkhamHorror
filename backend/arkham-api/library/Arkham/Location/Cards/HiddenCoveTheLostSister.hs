module Arkham.Location.Cards.HiddenCoveTheLostSister (hiddenCoveTheLostSister) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HiddenCoveTheLostSister = HiddenCoveTheLostSister LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenCoveTheLostSister :: LocationCard HiddenCoveTheLostSister
hiddenCoveTheLostSister = locationWith HiddenCoveTheLostSister Cards.hiddenCoveTheLostSister 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities HiddenCoveTheLostSister where
  getAbilities (HiddenCoveTheLostSister a) =
    extendRevealed a []

instance RunMessage HiddenCoveTheLostSister where
  runMessage msg (HiddenCoveTheLostSister attrs) = runQueueT $ case msg of
    _ -> HiddenCoveTheLostSister <$> liftRunMessage msg attrs

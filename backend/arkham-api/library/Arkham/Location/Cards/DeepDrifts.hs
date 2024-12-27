module Arkham.Location.Cards.DeepDrifts (deepDrifts) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DeepDrifts = DeepDrifts LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepDrifts :: LocationCard DeepDrifts
deepDrifts = locationWith DeepDrifts Cards.deepDrifts 1 (PerPlayer 3) (connectsToL .~ adjacentLocations)

instance HasAbilities DeepDrifts where
  getAbilities (DeepDrifts attrs) =
    extendRevealed attrs []

instance RunMessage DeepDrifts where
  runMessage msg (DeepDrifts attrs) = runQueueT $ case msg of
    _ -> DeepDrifts <$> liftRunMessage msg attrs

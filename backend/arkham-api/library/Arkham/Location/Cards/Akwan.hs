module Arkham.Location.Cards.Akwan (akwan) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Akwan = Akwan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

akwan :: LocationCard Akwan
akwan = locationWith Akwan Cards.akwan 3 (Static 0) connectsToAdjacent

instance HasAbilities Akwan where
  getAbilities (Akwan a) =
    extendRevealed a []

instance RunMessage Akwan where
  runMessage msg (Akwan attrs) = runQueueT $ case msg of
    _ -> Akwan <$> liftRunMessage msg attrs

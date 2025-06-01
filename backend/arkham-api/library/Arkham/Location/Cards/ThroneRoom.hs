module Arkham.Location.Cards.ThroneRoom (throneRoom) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ThroneRoom = ThroneRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throneRoom :: LocationCard ThroneRoom
throneRoom = location ThroneRoom Cards.throneRoom 5 (PerPlayer 1)

instance HasAbilities ThroneRoom where
  getAbilities (ThroneRoom attrs) =
    extendRevealed attrs []

instance RunMessage ThroneRoom where
  runMessage msg (ThroneRoom attrs) = runQueueT $ case msg of
    _ -> ThroneRoom <$> liftRunMessage msg attrs

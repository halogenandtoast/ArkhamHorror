module Arkham.Location.Cards.RightTurnB (rightTurnB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RightTurnB = RightTurnB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rightTurnB :: LocationCard RightTurnB
rightTurnB = location RightTurnB Cards.rightTurnB 2 (PerPlayer 1)

instance HasAbilities RightTurnB where
  getAbilities (RightTurnB a) =
    extendRevealed a []

instance RunMessage RightTurnB where
  runMessage msg (RightTurnB attrs) = runQueueT $ case msg of
    _ -> RightTurnB <$> liftRunMessage msg attrs

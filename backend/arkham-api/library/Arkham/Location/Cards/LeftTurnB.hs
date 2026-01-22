module Arkham.Location.Cards.LeftTurnB (leftTurnB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LeftTurnB = LeftTurnB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leftTurnB :: LocationCard LeftTurnB
leftTurnB = location LeftTurnB Cards.leftTurnB 2 (PerPlayer 1)

instance HasAbilities LeftTurnB where
  getAbilities (LeftTurnB a) =
    extendRevealed a []

instance RunMessage LeftTurnB where
  runMessage msg (LeftTurnB attrs) = runQueueT $ case msg of
    _ -> LeftTurnB <$> liftRunMessage msg attrs

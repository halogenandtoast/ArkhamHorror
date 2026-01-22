module Arkham.Location.Cards.LeftTurnA (leftTurnA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LeftTurnA = LeftTurnA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leftTurnA :: LocationCard LeftTurnA
leftTurnA = location LeftTurnA Cards.leftTurnA 2 (PerPlayer 1)

instance HasAbilities LeftTurnA where
  getAbilities (LeftTurnA a) =
    extendRevealed a []

instance RunMessage LeftTurnA where
  runMessage msg (LeftTurnA attrs) = runQueueT $ case msg of
    _ -> LeftTurnA <$> liftRunMessage msg attrs

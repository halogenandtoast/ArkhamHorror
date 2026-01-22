module Arkham.Location.Cards.RightTurnA (rightTurnA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RightTurnA = RightTurnA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rightTurnA :: LocationCard RightTurnA
rightTurnA = location RightTurnA Cards.rightTurnA 2 (PerPlayer 1)

instance HasAbilities RightTurnA where
  getAbilities (RightTurnA a) =
    extendRevealed a []

instance RunMessage RightTurnA where
  runMessage msg (RightTurnA attrs) = runQueueT $ case msg of
    _ -> RightTurnA <$> liftRunMessage msg attrs

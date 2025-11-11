module Arkham.Location.Cards.GuardRoom (guardRoom) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GuardRoom = GuardRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardRoom :: LocationCard GuardRoom
guardRoom = symbolLabel $ location GuardRoom Cards.guardRoom 0 (Static 0)

instance HasAbilities GuardRoom where
  getAbilities (GuardRoom attrs) =
    extendRevealed attrs []

instance RunMessage GuardRoom where
  runMessage msg (GuardRoom attrs) = runQueueT $ case msg of
    _ -> GuardRoom <$> liftRunMessage msg attrs

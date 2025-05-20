module Arkham.Location.Cards.ReturnToNotreDame (returnToNotreDame) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToNotreDame = ReturnToNotreDame LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToNotreDame :: LocationCard ReturnToNotreDame
returnToNotreDame = location ReturnToNotreDame Cards.returnToNotreDame 4 (PerPlayer 1)

instance HasAbilities ReturnToNotreDame where
  getAbilities (ReturnToNotreDame attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToNotreDame where
  runMessage msg (ReturnToNotreDame attrs) = runQueueT $ case msg of
    _ -> ReturnToNotreDame <$> liftRunMessage msg attrs

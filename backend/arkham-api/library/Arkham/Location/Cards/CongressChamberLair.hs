module Arkham.Location.Cards.CongressChamberLair (congressChamberLair) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CongressChamberLair = CongressChamberLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congressChamberLair :: LocationCard CongressChamberLair
congressChamberLair = location CongressChamberLair Cards.congressChamberLair 0 (Static 0)

instance HasAbilities CongressChamberLair where
  getAbilities (CongressChamberLair a) =
    extendRevealed a []

instance RunMessage CongressChamberLair where
  runMessage msg (CongressChamberLair attrs) = runQueueT $ case msg of
    _ -> CongressChamberLair <$> liftRunMessage msg attrs

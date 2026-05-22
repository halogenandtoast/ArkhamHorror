module Arkham.Location.Cards.SaltChamber (saltChamber) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SaltChamber = SaltChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saltChamber :: LocationCard SaltChamber
saltChamber = symbolLabel $ location SaltChamber Cards.saltChamber 4 (PerPlayer 3)

instance HasAbilities SaltChamber where
  getAbilities (SaltChamber a) =
    extendRevealed a []

instance RunMessage SaltChamber where
  runMessage msg (SaltChamber attrs) = runQueueT $ case msg of
    _ -> SaltChamber <$> liftRunMessage msg attrs

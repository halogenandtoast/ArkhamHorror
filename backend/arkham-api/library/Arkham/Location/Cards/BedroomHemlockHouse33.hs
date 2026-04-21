module Arkham.Location.Cards.BedroomHemlockHouse33 (bedroomHemlockHouse33) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BedroomHemlockHouse33 = BedroomHemlockHouse33 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomHemlockHouse33 :: LocationCard BedroomHemlockHouse33
bedroomHemlockHouse33 = symbolLabel $ location BedroomHemlockHouse33 Cards.bedroomHemlockHouse33 0 (PerPlayer 1)

instance HasAbilities BedroomHemlockHouse33 where
  getAbilities (BedroomHemlockHouse33 a) =
    extendRevealed a []

instance RunMessage BedroomHemlockHouse33 where
  runMessage msg (BedroomHemlockHouse33 attrs) = runQueueT $ case msg of
    _ -> BedroomHemlockHouse33 <$> liftRunMessage msg attrs

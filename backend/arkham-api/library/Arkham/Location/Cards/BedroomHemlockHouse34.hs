module Arkham.Location.Cards.BedroomHemlockHouse34 (bedroomHemlockHouse34) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BedroomHemlockHouse34 = BedroomHemlockHouse34 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomHemlockHouse34 :: LocationCard BedroomHemlockHouse34
bedroomHemlockHouse34 = symbolLabel $ location BedroomHemlockHouse34 Cards.bedroomHemlockHouse34 0 (PerPlayer 1)

instance HasAbilities BedroomHemlockHouse34 where
  getAbilities (BedroomHemlockHouse34 a) =
    extendRevealed a []

instance RunMessage BedroomHemlockHouse34 where
  runMessage msg (BedroomHemlockHouse34 attrs) = runQueueT $ case msg of
    _ -> BedroomHemlockHouse34 <$> liftRunMessage msg attrs

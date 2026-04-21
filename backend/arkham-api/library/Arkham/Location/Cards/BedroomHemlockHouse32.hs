module Arkham.Location.Cards.BedroomHemlockHouse32 (bedroomHemlockHouse32) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BedroomHemlockHouse32 = BedroomHemlockHouse32 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomHemlockHouse32 :: LocationCard BedroomHemlockHouse32
bedroomHemlockHouse32 = symbolLabel $ location BedroomHemlockHouse32 Cards.bedroomHemlockHouse32 0 (PerPlayer 1)

instance HasAbilities BedroomHemlockHouse32 where
  getAbilities (BedroomHemlockHouse32 a) =
    extendRevealed a []

instance RunMessage BedroomHemlockHouse32 where
  runMessage msg (BedroomHemlockHouse32 attrs) = runQueueT $ case msg of
    _ -> BedroomHemlockHouse32 <$> liftRunMessage msg attrs

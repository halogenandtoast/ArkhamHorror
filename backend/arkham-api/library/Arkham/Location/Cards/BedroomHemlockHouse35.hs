module Arkham.Location.Cards.BedroomHemlockHouse35 (bedroomHemlockHouse35) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BedroomHemlockHouse35 = BedroomHemlockHouse35 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomHemlockHouse35 :: LocationCard BedroomHemlockHouse35
bedroomHemlockHouse35 = locationWith BedroomHemlockHouse35 Cards.bedroomHemlockHouse35 0 (PerPlayer 1) connectsToAdjacent

instance HasAbilities BedroomHemlockHouse35 where
  getAbilities (BedroomHemlockHouse35 a) =
    extendRevealed a []

instance RunMessage BedroomHemlockHouse35 where
  runMessage msg (BedroomHemlockHouse35 attrs) = runQueueT $ case msg of
    _ -> BedroomHemlockHouse35 <$> liftRunMessage msg attrs

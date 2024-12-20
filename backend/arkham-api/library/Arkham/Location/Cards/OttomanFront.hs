module Arkham.Location.Cards.OttomanFront (ottomanFront) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OttomanFront = OttomanFront LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ottomanFront :: LocationCard OttomanFront
ottomanFront = location OttomanFront Cards.ottomanFront 4 (PerPlayer 2)

instance HasAbilities OttomanFront where
  getAbilities (OttomanFront attrs) =
    extendRevealed attrs []

instance RunMessage OttomanFront where
  runMessage msg (OttomanFront attrs) = runQueueT $ case msg of
    _ -> OttomanFront <$> liftRunMessage msg attrs

module Arkham.Location.Cards.RiverviewTheatre (riverviewTheatre) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RiverviewTheatre = RiverviewTheatre LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverviewTheatre :: LocationCard RiverviewTheatre
riverviewTheatre = location RiverviewTheatre Cards.riverviewTheatre 1 (PerPlayer 4)

instance HasAbilities RiverviewTheatre where
  getAbilities (RiverviewTheatre attrs) =
    extendRevealed attrs []

instance RunMessage RiverviewTheatre where
  runMessage msg (RiverviewTheatre attrs) = runQueueT $ case msg of
    _ -> RiverviewTheatre <$> liftRunMessage msg attrs

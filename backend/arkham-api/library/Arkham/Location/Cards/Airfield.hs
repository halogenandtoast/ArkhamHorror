module Arkham.Location.Cards.Airfield (airfield) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Airfield = Airfield LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

airfield :: LocationCard Airfield
airfield = location Airfield Cards.airfield 4 (PerPlayer 2)

instance HasAbilities Airfield where
  getAbilities (Airfield attrs) =
    extendRevealed attrs []

instance RunMessage Airfield where
  runMessage msg (Airfield attrs) = runQueueT $ case msg of
    _ -> Airfield <$> liftRunMessage msg attrs

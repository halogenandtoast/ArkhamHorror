module Arkham.Location.Cards.NarrowRidge (narrowRidge) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype NarrowRidge = NarrowRidge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowRidge :: LocationCard NarrowRidge
narrowRidge = location NarrowRidge Cards.narrowRidge 2 (PerPlayer 2)

instance HasAbilities NarrowRidge where
  getAbilities (NarrowRidge attrs) =
    extendRevealed attrs []

instance RunMessage NarrowRidge where
  runMessage msg (NarrowRidge attrs) = runQueueT $ case msg of
    _ -> NarrowRidge <$> liftRunMessage msg attrs

module Arkham.Location.Cards.MirrorNest10666 (mirrorNest10666) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MirrorNest10666 = MirrorNest10666 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest10666 :: LocationCard MirrorNest10666
mirrorNest10666 = location MirrorNest10666 Cards.mirrorNest10666 3 (Static 2)

instance RunMessage MirrorNest10666 where
  runMessage msg (MirrorNest10666 attrs) =
    runQueueT $ MirrorNest10666 <$> liftRunMessage msg attrs

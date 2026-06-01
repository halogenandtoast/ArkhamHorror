module Arkham.Location.Cards.MirrorNest10667 (mirrorNest10667) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MirrorNest10667 = MirrorNest10667 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest10667 :: LocationCard MirrorNest10667
mirrorNest10667 = location MirrorNest10667 Cards.mirrorNest10667 3 (Static 2)

instance RunMessage MirrorNest10667 where
  runMessage msg (MirrorNest10667 attrs) =
    runQueueT $ MirrorNest10667 <$> liftRunMessage msg attrs

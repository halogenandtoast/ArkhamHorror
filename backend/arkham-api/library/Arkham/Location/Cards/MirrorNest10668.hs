module Arkham.Location.Cards.MirrorNest10668 (mirrorNest10668) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MirrorNest10668 = MirrorNest10668 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest10668 :: LocationCard MirrorNest10668
mirrorNest10668 = location MirrorNest10668 Cards.mirrorNest10668 3 (Static 2)

instance RunMessage MirrorNest10668 where
  runMessage msg (MirrorNest10668 attrs) =
    runQueueT $ MirrorNest10668 <$> liftRunMessage msg attrs

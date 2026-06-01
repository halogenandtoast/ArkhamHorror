module Arkham.Location.Cards.MirrorNest10669 (mirrorNest10669) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MirrorNest10669 = MirrorNest10669 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest10669 :: LocationCard MirrorNest10669
mirrorNest10669 = location MirrorNest10669 Cards.mirrorNest10669 3 (Static 2)

instance RunMessage MirrorNest10669 where
  runMessage msg (MirrorNest10669 attrs) =
    runQueueT $ MirrorNest10669 <$> liftRunMessage msg attrs

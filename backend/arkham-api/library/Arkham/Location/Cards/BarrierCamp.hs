module Arkham.Location.Cards.BarrierCamp (barrierCamp, BarrierCamp (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BarrierCamp = BarrierCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierCamp :: LocationCard BarrierCamp
barrierCamp = symbolLabel $ location BarrierCamp Cards.barrierCamp 0 (Static 0)

instance HasAbilities BarrierCamp where
  getAbilities (BarrierCamp attrs) =
    extendRevealed attrs []

instance RunMessage BarrierCamp where
  runMessage msg (BarrierCamp attrs) = runQueueT $ case msg of
    _ -> BarrierCamp <$> liftRunMessage msg attrs

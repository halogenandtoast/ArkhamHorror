module Arkham.Location.Cards.PumpRoom
  ( pumpRoom
  , PumpRoom(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PumpRoom = PumpRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pumpRoom :: LocationCard PumpRoom
pumpRoom = location PumpRoom Cards.pumpRoom 0 (Static 0)

instance HasAbilities PumpRoom where
  getAbilities (PumpRoom attrs) =
    extendRevealed attrs []

instance RunMessage PumpRoom where
  runMessage msg (PumpRoom attrs) = runQueueT $ case msg of
    _ -> PumpRoom <$> liftRunMessage msg attrs

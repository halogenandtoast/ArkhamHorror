module Arkham.Location.Cards.LanternRoom
  ( lanternRoom
  , LanternRoom(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LanternRoom = LanternRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lanternRoom :: LocationCard LanternRoom
lanternRoom = location LanternRoom Cards.lanternRoom 0 (Static 0)

instance HasAbilities LanternRoom where
  getAbilities (LanternRoom attrs) =
    extendRevealed attrs []

instance RunMessage LanternRoom where
  runMessage msg (LanternRoom attrs) = runQueueT $ case msg of
    _ -> LanternRoom <$> liftRunMessage msg attrs

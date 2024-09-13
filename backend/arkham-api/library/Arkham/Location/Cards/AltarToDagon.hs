module Arkham.Location.Cards.AltarToDagon (
  altarToDagon,
  AltarToDagon (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype AltarToDagon = AltarToDagon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

altarToDagon :: LocationCard AltarToDagon
altarToDagon = locationWith AltarToDagon Cards.altarToDagon 3 (Static 0) connectsToAdjacent

instance HasAbilities AltarToDagon where
  getAbilities (AltarToDagon attrs) =
    extendRevealed attrs []

instance RunMessage AltarToDagon where
  runMessage msg (AltarToDagon attrs) = runQueueT $ case msg of
    _ -> AltarToDagon <$> liftRunMessage msg attrs

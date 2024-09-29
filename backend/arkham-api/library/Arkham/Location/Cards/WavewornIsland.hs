module Arkham.Location.Cards.WavewornIsland (
  wavewornIsland,
  WavewornIsland (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WavewornIsland = WavewornIsland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wavewornIsland :: LocationCard WavewornIsland
wavewornIsland = location WavewornIsland Cards.wavewornIsland 4 (Static 0)

instance HasAbilities WavewornIsland where
  getAbilities (WavewornIsland attrs) =
    extendRevealed attrs []

instance RunMessage WavewornIsland where
  runMessage msg (WavewornIsland attrs) = runQueueT $ case msg of
    _ -> WavewornIsland <$> liftRunMessage msg attrs

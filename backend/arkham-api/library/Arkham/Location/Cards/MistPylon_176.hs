module Arkham.Location.Cards.MistPylon_176 (mistPylon_176) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MistPylon_176 = MistPylon_176 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistPylon_176 :: LocationCard MistPylon_176
mistPylon_176 = location MistPylon_176 Cards.mistPylon_176 0 (Static 0)

instance HasAbilities MistPylon_176 where
  getAbilities (MistPylon_176 attrs) =
    extendRevealed attrs []

instance RunMessage MistPylon_176 where
  runMessage msg (MistPylon_176 attrs) = runQueueT $ case msg of
    _ -> MistPylon_176 <$> liftRunMessage msg attrs

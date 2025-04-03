module Arkham.Location.Cards.MistPylon_177 (mistPylon_177) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MistPylon_177 = MistPylon_177 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistPylon_177 :: LocationCard MistPylon_177
mistPylon_177 = location MistPylon_177 Cards.mistPylon_177 0 (Static 0)

instance HasAbilities MistPylon_177 where
  getAbilities (MistPylon_177 attrs) =
    extendRevealed attrs []

instance RunMessage MistPylon_177 where
  runMessage msg (MistPylon_177 attrs) = runQueueT $ case msg of
    _ -> MistPylon_177 <$> liftRunMessage msg attrs

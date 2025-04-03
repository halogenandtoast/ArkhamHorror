module Arkham.Location.Cards.MistPylon_178 (mistPylon_178) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MistPylon_178 = MistPylon_178 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistPylon_178 :: LocationCard MistPylon_178
mistPylon_178 = location MistPylon_178 Cards.mistPylon_178 0 (Static 0)

instance HasAbilities MistPylon_178 where
  getAbilities (MistPylon_178 attrs) =
    extendRevealed attrs []

instance RunMessage MistPylon_178 where
  runMessage msg (MistPylon_178 attrs) = runQueueT $ case msg of
    _ -> MistPylon_178 <$> liftRunMessage msg attrs

module Arkham.Location.Cards.MistPylon_174 (mistPylon_174) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MistPylon_174 = MistPylon_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistPylon_174 :: LocationCard MistPylon_174
mistPylon_174 = location MistPylon_174 Cards.mistPylon_174 0 (Static 0)

instance HasAbilities MistPylon_174 where
  getAbilities (MistPylon_174 attrs) =
    extendRevealed attrs []

instance RunMessage MistPylon_174 where
  runMessage msg (MistPylon_174 attrs) = runQueueT $ case msg of
    _ -> MistPylon_174 <$> liftRunMessage msg attrs

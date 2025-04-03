module Arkham.Location.Cards.MistPylon_175 (mistPylon_175) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MistPylon_175 = MistPylon_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistPylon_175 :: LocationCard MistPylon_175
mistPylon_175 = location MistPylon_175 Cards.mistPylon_175 0 (Static 0)

instance HasAbilities MistPylon_175 where
  getAbilities (MistPylon_175 attrs) =
    extendRevealed attrs []

instance RunMessage MistPylon_175 where
  runMessage msg (MistPylon_175 attrs) = runQueueT $ case msg of
    _ -> MistPylon_175 <$> liftRunMessage msg attrs

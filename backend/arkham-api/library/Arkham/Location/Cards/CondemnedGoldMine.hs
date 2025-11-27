module Arkham.Location.Cards.CondemnedGoldMine (condemnedGoldMine) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CondemnedGoldMine = CondemnedGoldMine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

condemnedGoldMine :: LocationCard CondemnedGoldMine
condemnedGoldMine = location CondemnedGoldMine Cards.condemnedGoldMine 6 (Static 0)

instance HasAbilities CondemnedGoldMine where
  getAbilities (CondemnedGoldMine attrs) =
    extendRevealed attrs []

instance RunMessage CondemnedGoldMine where
  runMessage msg (CondemnedGoldMine attrs) = runQueueT $ case msg of
    _ -> CondemnedGoldMine <$> liftRunMessage msg attrs

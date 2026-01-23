module Arkham.Location.Cards.AlkalineRailA (alkalineRailA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AlkalineRailA = AlkalineRailA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alkalineRailA :: LocationCard AlkalineRailA
alkalineRailA = location AlkalineRailA Cards.alkalineRailA 3 (PerPlayer 3)

instance HasAbilities AlkalineRailA where
  getAbilities (AlkalineRailA a) =
    extendRevealed a []

instance RunMessage AlkalineRailA where
  runMessage msg (AlkalineRailA attrs) = runQueueT $ case msg of
    _ -> AlkalineRailA <$> liftRunMessage msg attrs

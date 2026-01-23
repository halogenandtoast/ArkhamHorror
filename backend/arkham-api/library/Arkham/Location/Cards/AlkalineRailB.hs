module Arkham.Location.Cards.AlkalineRailB (alkalineRailB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AlkalineRailB = AlkalineRailB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alkalineRailB :: LocationCard AlkalineRailB
alkalineRailB = symbolLabel $ location AlkalineRailB Cards.alkalineRailB 3 (PerPlayer 3)

instance HasAbilities AlkalineRailB where
  getAbilities (AlkalineRailB a) =
    extendRevealed a []

instance RunMessage AlkalineRailB where
  runMessage msg (AlkalineRailB attrs) = runQueueT $ case msg of
    _ -> AlkalineRailB <$> liftRunMessage msg attrs

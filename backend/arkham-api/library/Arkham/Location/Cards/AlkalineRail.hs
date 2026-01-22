module Arkham.Location.Cards.AlkalineRail (alkalineRail) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AlkalineRail = AlkalineRail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alkalineRail :: LocationCard AlkalineRail
alkalineRail = location AlkalineRail Cards.alkalineRail 3 (PerPlayer 3)

instance HasAbilities AlkalineRail where
  getAbilities (AlkalineRail a) =
    extendRevealed a []

instance RunMessage AlkalineRail where
  runMessage msg (AlkalineRail attrs) = runQueueT $ case msg of
    _ -> AlkalineRail <$> liftRunMessage msg attrs

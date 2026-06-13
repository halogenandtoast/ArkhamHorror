module Arkham.Location.Cards.CoralReefFeedingGrounds (coralReefFeedingGrounds) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CoralReefFeedingGrounds = CoralReefFeedingGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coralReefFeedingGrounds :: LocationCard CoralReefFeedingGrounds
coralReefFeedingGrounds = location CoralReefFeedingGrounds Cards.coralReefFeedingGrounds 4 (Static 3)

-- TODO: abilities

instance RunMessage CoralReefFeedingGrounds where
  runMessage msg (CoralReefFeedingGrounds attrs) = runQueueT $ CoralReefFeedingGrounds <$> liftRunMessage msg attrs

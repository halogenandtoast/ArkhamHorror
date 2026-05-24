module Arkham.Location.Cards.RockyShoreline (rockyShoreline) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RockyShoreline = RockyShoreline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rockyShoreline :: LocationCard RockyShoreline
rockyShoreline = locationWith RockyShoreline Cards.rockyShoreline 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RockyShoreline where
  getAbilities (RockyShoreline a) =
    extendRevealed a []

instance RunMessage RockyShoreline where
  runMessage msg (RockyShoreline attrs) = runQueueT $ case msg of
    _ -> RockyShoreline <$> liftRunMessage msg attrs

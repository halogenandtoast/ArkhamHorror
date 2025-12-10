module Arkham.Location.Cards.RamblingRouteB (ramblingRouteB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RamblingRouteB = RamblingRouteB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteB :: LocationCard RamblingRouteB
ramblingRouteB = symbolLabel $ locationWith RamblingRouteB Cards.ramblingRouteB 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RamblingRouteB where
  getAbilities (RamblingRouteB a) =
    extendRevealed a []

instance RunMessage RamblingRouteB where
  runMessage msg (RamblingRouteB attrs) = runQueueT $ case msg of
    _ -> RamblingRouteB <$> liftRunMessage msg attrs

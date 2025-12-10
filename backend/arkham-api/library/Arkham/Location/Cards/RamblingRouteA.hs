module Arkham.Location.Cards.RamblingRouteA (ramblingRouteA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RamblingRouteA = RamblingRouteA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteA :: LocationCard RamblingRouteA
ramblingRouteA = symbolLabel $ locationWith RamblingRouteA Cards.ramblingRouteA 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RamblingRouteA where
  getAbilities (RamblingRouteA a) =
    extendRevealed a []

instance RunMessage RamblingRouteA where
  runMessage msg (RamblingRouteA attrs) = runQueueT $ case msg of
    _ -> RamblingRouteA <$> liftRunMessage msg attrs

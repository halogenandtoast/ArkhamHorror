module Arkham.Location.Cards.RamblingRouteC (ramblingRouteC) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RamblingRouteC = RamblingRouteC LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteC :: LocationCard RamblingRouteC
ramblingRouteC = symbolLabel $ locationWith RamblingRouteC Cards.ramblingRouteC 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RamblingRouteC where
  getAbilities (RamblingRouteC a) =
    extendRevealed a []

instance RunMessage RamblingRouteC where
  runMessage msg (RamblingRouteC attrs) = runQueueT $ case msg of
    _ -> RamblingRouteC <$> liftRunMessage msg attrs

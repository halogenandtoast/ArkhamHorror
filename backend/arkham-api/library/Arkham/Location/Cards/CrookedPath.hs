module Arkham.Location.Cards.CrookedPath (crookedPath) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CrookedPath = CrookedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crookedPath :: LocationCard CrookedPath
crookedPath = locationWith CrookedPath Cards.crookedPath 0 (Static 1) connectsToAdjacent

instance HasAbilities CrookedPath where
  getAbilities (CrookedPath a) =
    extendRevealed a []

instance RunMessage CrookedPath where
  runMessage msg (CrookedPath attrs) = runQueueT $ case msg of
    _ -> CrookedPath <$> liftRunMessage msg attrs

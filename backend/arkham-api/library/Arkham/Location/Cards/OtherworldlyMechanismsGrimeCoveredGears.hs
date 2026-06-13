module Arkham.Location.Cards.OtherworldlyMechanismsGrimeCoveredGears (otherworldlyMechanismsGrimeCoveredGears) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OtherworldlyMechanismsGrimeCoveredGears = OtherworldlyMechanismsGrimeCoveredGears LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsGrimeCoveredGears :: LocationCard OtherworldlyMechanismsGrimeCoveredGears
otherworldlyMechanismsGrimeCoveredGears = location OtherworldlyMechanismsGrimeCoveredGears Cards.otherworldlyMechanismsGrimeCoveredGears 3 (Static 1)

-- TODO: abilities

instance RunMessage OtherworldlyMechanismsGrimeCoveredGears where
  runMessage msg (OtherworldlyMechanismsGrimeCoveredGears attrs) = runQueueT $ OtherworldlyMechanismsGrimeCoveredGears <$> liftRunMessage msg attrs

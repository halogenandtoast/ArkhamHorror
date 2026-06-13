module Arkham.Location.Cards.OtherworldlyMechanismsInscrutableApparatus (otherworldlyMechanismsInscrutableApparatus) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OtherworldlyMechanismsInscrutableApparatus = OtherworldlyMechanismsInscrutableApparatus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsInscrutableApparatus :: LocationCard OtherworldlyMechanismsInscrutableApparatus
otherworldlyMechanismsInscrutableApparatus = location OtherworldlyMechanismsInscrutableApparatus Cards.otherworldlyMechanismsInscrutableApparatus 4 (Static 1)

-- TODO: abilities

instance RunMessage OtherworldlyMechanismsInscrutableApparatus where
  runMessage msg (OtherworldlyMechanismsInscrutableApparatus attrs) = runQueueT $ OtherworldlyMechanismsInscrutableApparatus <$> liftRunMessage msg attrs

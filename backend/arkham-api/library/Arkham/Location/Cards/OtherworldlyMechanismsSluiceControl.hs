module Arkham.Location.Cards.OtherworldlyMechanismsSluiceControl (otherworldlyMechanismsSluiceControl) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OtherworldlyMechanismsSluiceControl = OtherworldlyMechanismsSluiceControl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsSluiceControl :: LocationCard OtherworldlyMechanismsSluiceControl
otherworldlyMechanismsSluiceControl = location OtherworldlyMechanismsSluiceControl Cards.otherworldlyMechanismsSluiceControl 1 (Static 3)

-- TODO: abilities

instance RunMessage OtherworldlyMechanismsSluiceControl where
  runMessage msg (OtherworldlyMechanismsSluiceControl attrs) = runQueueT $ OtherworldlyMechanismsSluiceControl <$> liftRunMessage msg attrs

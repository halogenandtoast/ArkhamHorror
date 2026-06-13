module Arkham.Location.Cards.AcidicCoelom (acidicCoelom) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AcidicCoelom = AcidicCoelom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acidicCoelom :: LocationCard AcidicCoelom
acidicCoelom = location AcidicCoelom Cards.acidicCoelom 4 (Static 1)

-- TODO: abilities

instance RunMessage AcidicCoelom where
  runMessage msg (AcidicCoelom attrs) = runQueueT $ AcidicCoelom <$> liftRunMessage msg attrs

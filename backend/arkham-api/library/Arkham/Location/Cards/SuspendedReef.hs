module Arkham.Location.Cards.SuspendedReef (suspendedReef) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SuspendedReef = SuspendedReef LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspendedReef :: LocationCard SuspendedReef
suspendedReef = location SuspendedReef Cards.suspendedReef 3 (Static 2)

-- TODO: abilities

instance RunMessage SuspendedReef where
  runMessage msg (SuspendedReef attrs) = runQueueT $ SuspendedReef <$> liftRunMessage msg attrs

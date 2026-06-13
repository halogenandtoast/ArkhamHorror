module Arkham.Location.Cards.CorruptedVault (corruptedVault) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CorruptedVault = CorruptedVault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corruptedVault :: LocationCard CorruptedVault
corruptedVault = location CorruptedVault Cards.corruptedVault 3 (Static 1)

-- TODO: abilities

instance RunMessage CorruptedVault where
  runMessage msg (CorruptedVault attrs) = runQueueT $ CorruptedVault <$> liftRunMessage msg attrs

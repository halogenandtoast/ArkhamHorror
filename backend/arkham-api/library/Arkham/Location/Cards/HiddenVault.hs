module Arkham.Location.Cards.HiddenVault (hiddenVault) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HiddenVault = HiddenVault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenVault :: LocationCard HiddenVault
hiddenVault = location HiddenVault Cards.hiddenVault 3 (Static 1)

-- TODO: abilities

instance RunMessage HiddenVault where
  runMessage msg (HiddenVault attrs) = runQueueT $ HiddenVault <$> liftRunMessage msg attrs

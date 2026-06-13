module Arkham.Location.Cards.UnderseaVault (underseaVault) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype UnderseaVault = UnderseaVault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaVault :: LocationCard UnderseaVault
underseaVault = location UnderseaVault Cards.underseaVault 5 (Static 1)

-- TODO: abilities

instance RunMessage UnderseaVault where
  runMessage msg (UnderseaVault attrs) = runQueueT $ UnderseaVault <$> liftRunMessage msg attrs

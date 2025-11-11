module Arkham.Location.Cards.VaultDoor (vaultDoor) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype VaultDoor = VaultDoor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultDoor :: LocationCard VaultDoor
vaultDoor = symbolLabel $ location VaultDoor Cards.vaultDoor 0 (Static 0)

instance HasAbilities VaultDoor where
  getAbilities (VaultDoor attrs) =
    extendRevealed attrs []

instance RunMessage VaultDoor where
  runMessage msg (VaultDoor attrs) = runQueueT $ case msg of
    _ -> VaultDoor <$> liftRunMessage msg attrs

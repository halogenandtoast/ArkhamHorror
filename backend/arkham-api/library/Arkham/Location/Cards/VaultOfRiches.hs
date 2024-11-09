module Arkham.Location.Cards.VaultOfRiches
  ( vaultOfRiches
  , VaultOfRiches(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype VaultOfRiches = VaultOfRiches LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultOfRiches :: LocationCard VaultOfRiches
vaultOfRiches = location VaultOfRiches Cards.vaultOfRiches 4 (PerPlayer 2)

instance HasAbilities VaultOfRiches where
  getAbilities (VaultOfRiches attrs) =
    extendRevealed attrs []

instance RunMessage VaultOfRiches where
  runMessage msg (VaultOfRiches attrs) = runQueueT $ case msg of
    _ -> VaultOfRiches <$> liftRunMessage msg attrs

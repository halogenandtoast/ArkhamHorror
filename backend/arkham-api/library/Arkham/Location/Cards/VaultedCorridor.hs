module Arkham.Location.Cards.VaultedCorridor (vaultedCorridor) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype VaultedCorridor = VaultedCorridor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultedCorridor :: LocationCard VaultedCorridor
vaultedCorridor = location VaultedCorridor Cards.vaultedCorridor 3 (PerPlayer 1)

instance HasAbilities VaultedCorridor where
  getAbilities (VaultedCorridor attrs) =
    extendRevealed attrs []

instance RunMessage VaultedCorridor where
  runMessage msg (VaultedCorridor attrs) = runQueueT $ case msg of
    _ -> VaultedCorridor <$> liftRunMessage msg attrs

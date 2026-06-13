module Arkham.Treachery.Cards.AncientVaultG (ancientVaultG) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultG = AncientVaultG TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultG :: TreacheryCard AncientVaultG
ancientVaultG = treachery AncientVaultG Cards.ancientVaultG

-- TODO: abilities
instance RunMessage AncientVaultG where
  runMessage msg (AncientVaultG attrs) = runQueueT $ AncientVaultG <$> liftRunMessage msg attrs

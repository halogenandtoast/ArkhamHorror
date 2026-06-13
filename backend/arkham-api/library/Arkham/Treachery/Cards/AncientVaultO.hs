module Arkham.Treachery.Cards.AncientVaultO (ancientVaultO) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultO = AncientVaultO TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultO :: TreacheryCard AncientVaultO
ancientVaultO = treachery AncientVaultO Cards.ancientVaultO

-- TODO: abilities
instance RunMessage AncientVaultO where
  runMessage msg (AncientVaultO attrs) = runQueueT $ AncientVaultO <$> liftRunMessage msg attrs

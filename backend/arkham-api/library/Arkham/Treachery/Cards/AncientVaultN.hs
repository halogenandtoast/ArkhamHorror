module Arkham.Treachery.Cards.AncientVaultN (ancientVaultN) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultN = AncientVaultN TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultN :: TreacheryCard AncientVaultN
ancientVaultN = treachery AncientVaultN Cards.ancientVaultN

-- TODO: abilities
instance RunMessage AncientVaultN where
  runMessage msg (AncientVaultN attrs) = runQueueT $ AncientVaultN <$> liftRunMessage msg attrs

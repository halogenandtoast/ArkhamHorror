module Arkham.Treachery.Cards.AncientVaultP (ancientVaultP) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultP = AncientVaultP TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultP :: TreacheryCard AncientVaultP
ancientVaultP = treachery AncientVaultP Cards.ancientVaultP

-- TODO: abilities
instance RunMessage AncientVaultP where
  runMessage msg (AncientVaultP attrs) = runQueueT $ AncientVaultP <$> liftRunMessage msg attrs

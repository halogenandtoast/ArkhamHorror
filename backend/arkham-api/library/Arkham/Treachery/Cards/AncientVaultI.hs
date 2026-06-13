module Arkham.Treachery.Cards.AncientVaultI (ancientVaultI) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultI = AncientVaultI TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultI :: TreacheryCard AncientVaultI
ancientVaultI = treachery AncientVaultI Cards.ancientVaultI

-- TODO: abilities
instance RunMessage AncientVaultI where
  runMessage msg (AncientVaultI attrs) = runQueueT $ AncientVaultI <$> liftRunMessage msg attrs

module Arkham.Treachery.Cards.SecretsLost (secretsLost) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecretsLost = SecretsLost TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsLost :: TreacheryCard SecretsLost
secretsLost = treachery SecretsLost Cards.secretsLost

instance RunMessage SecretsLost where
  runMessage msg t@(SecretsLost attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SecretsLost <$> liftRunMessage msg attrs

module Arkham.Act.Cards.SecretsAndLiesV1 (secretsAndLiesV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SecretsAndLiesV1 = SecretsAndLiesV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

secretsAndLiesV1 :: ActCard SecretsAndLiesV1
secretsAndLiesV1 = act (1, A) SecretsAndLiesV1 Cards.secretsAndLiesV1 Nothing

instance RunMessage SecretsAndLiesV1 where
  runMessage msg a@(SecretsAndLiesV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SecretsAndLiesV1 <$> liftRunMessage msg attrs

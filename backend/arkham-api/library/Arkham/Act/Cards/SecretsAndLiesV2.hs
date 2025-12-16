module Arkham.Act.Cards.SecretsAndLiesV2 (secretsAndLiesV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SecretsAndLiesV2 = SecretsAndLiesV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

secretsAndLiesV2 :: ActCard SecretsAndLiesV2
secretsAndLiesV2 = act (1, A) SecretsAndLiesV2 Cards.secretsAndLiesV2 Nothing

instance RunMessage SecretsAndLiesV2 where
  runMessage msg a@(SecretsAndLiesV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SecretsAndLiesV2 <$> liftRunMessage msg attrs

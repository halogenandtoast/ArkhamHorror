module Arkham.Act.Cards.SecretsAndLiesV3 (secretsAndLiesV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SecretsAndLiesV3 = SecretsAndLiesV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

secretsAndLiesV3 :: ActCard SecretsAndLiesV3
secretsAndLiesV3 = act (1, A) SecretsAndLiesV3 Cards.secretsAndLiesV3 Nothing

instance RunMessage SecretsAndLiesV3 where
  runMessage msg a@(SecretsAndLiesV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SecretsAndLiesV3 <$> liftRunMessage msg attrs

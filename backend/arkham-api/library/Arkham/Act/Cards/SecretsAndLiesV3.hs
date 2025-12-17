module Arkham.Act.Cards.SecretsAndLiesV3 (secretsAndLiesV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Modifier (setActiveDuringSetup)

newtype SecretsAndLiesV3 = SecretsAndLiesV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

secretsAndLiesV3 :: ActCard SecretsAndLiesV3
secretsAndLiesV3 = act (1, A) SecretsAndLiesV3 Cards.secretsAndLiesV3 Nothing

instance HasModifiersFor SecretsAndLiesV3 where
  getModifiersFor (SecretsAndLiesV3 a) = do
    modifySelectWith
      a
      (enemyIs Enemies.theRedGlovedManPurposeUnknown)
      setActiveDuringSetup
      [AddKeyword $ Keyword.Concealed TheRedGlovedMan (PerPlayer 1), CannotBeDamaged]

instance RunMessage SecretsAndLiesV3 where
  runMessage msg a@(SecretsAndLiesV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SecretsAndLiesV3 <$> liftRunMessage msg attrs

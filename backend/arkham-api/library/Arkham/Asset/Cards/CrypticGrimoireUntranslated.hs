module Arkham.Asset.Cards.CrypticGrimoireUntranslated (
  crypticGrimoireUntranslated,
  CrypticGrimoireUntranslated (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.ChaosBag
import Arkham.Matcher

newtype CrypticGrimoireUntranslated = CrypticGrimoireUntranslated AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticGrimoireUntranslated :: AssetCard CrypticGrimoireUntranslated
crypticGrimoireUntranslated =
  asset CrypticGrimoireUntranslated Cards.crypticGrimoireUntranslated

instance HasAbilities CrypticGrimoireUntranslated where
  getAbilities (CrypticGrimoireUntranslated x) =
    [ controlledAbility x 1 HasRemainingCurseTokens actionAbility
    , controlledAbility x 2 (ChaosTokenCountIs #curse $ atLeast 10)
        $ actionAbilityWithCost
        $ discardCost x
    ]

instance RunMessage CrypticGrimoireUntranslated where
  runMessage msg a@(CrypticGrimoireUntranslated attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      addCurseTokens 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      n <- min 5 <$> getRemainingBlessTokens
      pushAll $ Record YouHaveTranslatedTheGrimoire : replicate n (SwapChaosToken #curse #bless)
      pure a
    _ -> CrypticGrimoireUntranslated <$> liftRunMessage msg attrs

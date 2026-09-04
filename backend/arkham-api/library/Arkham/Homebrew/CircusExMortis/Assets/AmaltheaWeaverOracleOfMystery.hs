module Arkham.Homebrew.CircusExMortis.Assets.AmaltheaWeaverOracleOfMystery (
  amaltheaWeaverOracleOfMystery,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype AmaltheaWeaverOracleOfMystery = AmaltheaWeaverOracleOfMystery AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaltheaWeaverOracleOfMystery :: AssetCard AmaltheaWeaverOracleOfMystery
amaltheaWeaverOracleOfMystery =
  ally AmaltheaWeaverOracleOfMystery Cards.amaltheaWeaverOracleOfMystery (3, 3)

instance HasModifiersFor AmaltheaWeaverOracleOfMystery where
  getModifiersFor (AmaltheaWeaverOracleOfMystery a) =
    controllerGets a [SkillModifier #willpower 1]

instance HasAbilities AmaltheaWeaverOracleOfMystery where
  getAbilities (AmaltheaWeaverOracleOfMystery a) = amaltheaWeaverAbilities a

instance RunMessage AmaltheaWeaverOracleOfMystery where
  runMessage msg a@(AmaltheaWeaverOracleOfMystery attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amaltheaWeaverBoost attrs
      amaltheaWeaverRider attrs msg
      pure a
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      amaltheaWeaverChooseRecipient attrs (`forInvestigator` msg)
      pure a
    ForInvestigator iid (DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1)) -> do
      chooseOneM iid $ campaignI18n $ scope "amaltheaWeaverOracleOfMystery" do
        labeled "drawTopCard" $ drawCards iid (attrs.ability 1) 1
        labeled "drawBottomCard" $ drawCardFromBottom iid (attrs.ability 1)
      pure a
    _ -> AmaltheaWeaverOracleOfMystery <$> liftRunMessage msg attrs

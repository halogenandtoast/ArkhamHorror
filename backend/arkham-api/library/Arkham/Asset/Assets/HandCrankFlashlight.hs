module Arkham.Asset.Assets.HandCrankFlashlight (handCrankFlashlight) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype HandCrankFlashlight = HandCrankFlashlight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handCrankFlashlight :: AssetCard HandCrankFlashlight
handCrankFlashlight = asset HandCrankFlashlight Cards.handCrankFlashlight

instance HasAbilities HandCrankFlashlight where
  getAbilities (HandCrankFlashlight a) = [controlled_ a 1 investigateAction_]

instance RunMessage HandCrankFlashlight where
  runMessage msg a@(HandCrankFlashlight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 1)
      investigate_ sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withLocationOf iid \lid -> do
        chooseOneM iid do
          labeled "Discard to deal give your location -1 shroud until the end of the round" do
            toDiscardBy iid (attrs.ability 1) attrs
            roundModifier (attrs.ability 1) lid $ ShroudModifier (-1)
          withI18n skip_
      pure a
    _ -> HandCrankFlashlight <$> liftRunMessage msg attrs

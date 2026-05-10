module Arkham.Asset.Assets.OldCompass (oldCompass) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype OldCompass = OldCompass AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldCompass :: AssetCard OldCompass
oldCompass = asset OldCompass Cards.oldCompass

instance HasAbilities OldCompass where
  getAbilities (OldCompass a) = [controlled_ a 1 investigateAction_]

instance RunMessage OldCompass where
  runMessage msg a@(OldCompass attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      withLocationOf iid \loc ->
        skillTestModifier sid (attrs.ability 1) loc (ShroudModifier (-1))
      investigate_ sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when attrs.ready do
        withSkillTest \stid -> do
          sid <- getRandom
          chooseOneM iid do
            (cardI18n $ labeled' "oldCompass.exhaustOldCompass") do
              exhaustThis attrs
              push $ RepeatSkillTest sid stid
            withI18n skip_
      pure a
    _ -> OldCompass <$> liftRunMessage msg attrs

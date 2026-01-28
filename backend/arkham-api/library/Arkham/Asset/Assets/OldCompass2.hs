module Arkham.Asset.Assets.OldCompass2 (oldCompass2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype OldCompass2 = OldCompass2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldCompass2 :: AssetCard OldCompass2
oldCompass2 = asset OldCompass2 Cards.oldCompass2

instance HasAbilities OldCompass2 where
  getAbilities (OldCompass2 a) = [controlled_ a 1 investigateAction_]

instance RunMessage OldCompass2 where
  runMessage msg a@(OldCompass2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      withLocationOf iid \loc ->
        skillTestModifier sid (attrs.ability 1) loc (ShroudModifier (-1))
      investigate_ sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTest \stid -> do
        sid <- getRandom
        mloc <- getSkillTestTargetedLocation
        chooseOneM iid do
          labeled "Exhaust Old Compass" do
            exhaustThis attrs
            -- the -1 from above will be retained so we add another -1 to get to -2
            for_ mloc \loc -> skillTestModifier sid (attrs.ability 1) loc (ShroudModifier (-1))
            push $ RepeatSkillTest sid stid
          withI18n skip_
      pure a
    _ -> OldCompass2 <$> liftRunMessage msg attrs

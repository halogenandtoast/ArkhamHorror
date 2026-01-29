module Arkham.Asset.Assets.BrokenBottle (brokenBottle) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype BrokenBottle = BrokenBottle AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenBottle :: AssetCard BrokenBottle
brokenBottle = asset BrokenBottle Cards.brokenBottle

instance HasAbilities BrokenBottle where
  getAbilities (BrokenBottle a) = [controlled_ a 1 fightAction_]

instance RunMessage BrokenBottle where
  runMessage msg a@(BrokenBottle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Discard to deal +1 damage" do
            toDiscardBy iid (attrs.ability 1) attrs
            skillTestModifier sid (attrs.ability 1) iid $ DamageDealt 1
          withI18n skip_
      pure a
    _ -> BrokenBottle <$> liftRunMessage msg attrs

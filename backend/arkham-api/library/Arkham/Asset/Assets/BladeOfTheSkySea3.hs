module Arkham.Asset.Assets.BladeOfTheSkySea3 (bladeOfTheSkySea3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy, isSkillTestSource)
import Arkham.Matcher

newtype BladeOfTheSkySea3 = BladeOfTheSkySea3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bladeOfTheSkySea3 :: AssetCard BladeOfTheSkySea3
bladeOfTheSkySea3 = asset BladeOfTheSkySea3 Cards.bladeOfTheSkySea3

instance HasModifiersFor BladeOfTheSkySea3 where
  getModifiersFor (BladeOfTheSkySea3 a) = for_ a.controller \iid -> do
    maybeModified_ a iid do
      liftGuardM $ isSkillTestSource (a.ability 1)
      eid <- MaybeT getSkillTestTargetedEnemy
      liftGuardM $ eid <=~> ExhaustedEnemy
      pure [DamageDealt 1]

instance HasAbilities BladeOfTheSkySea3 where
  getAbilities (BladeOfTheSkySea3 a) = [controlled_ a 1 fightAction_]

instance RunMessage BladeOfTheSkySea3 where
  runMessage msg a@(BladeOfTheSkySea3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemyWithModifiers sid iid (attrs.ability 1) [SkillModifier #combat 2]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> BladeOfTheSkySea3 <$> liftRunMessage msg attrs

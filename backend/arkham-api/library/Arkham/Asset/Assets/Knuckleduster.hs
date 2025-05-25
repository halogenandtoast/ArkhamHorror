module Arkham.Asset.Assets.Knuckleduster (knuckleduster) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestSource, getSkillTestTargetedEnemy)
import Arkham.Keyword qualified as Keyword

newtype Knuckleduster = Knuckleduster AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knuckleduster :: AssetCard Knuckleduster
knuckleduster = asset Knuckleduster Cards.knuckleduster

instance HasAbilities Knuckleduster where
  getAbilities (Knuckleduster a) = [fightAbility a 1 mempty ControlsThis]

instance HasModifiersFor Knuckleduster where
  getModifiersFor (Knuckleduster attrs) =
    getSkillTestTargetedEnemy >>= traverse_ \eid -> do
      maybeModified_ attrs eid do
        guardM $ isAbilitySource attrs 1 <$> MaybeT getSkillTestSource
        Action.Fight <- MaybeT getSkillTestAction
        pure [AddKeyword Keyword.Retaliate]

instance RunMessage Knuckleduster where
  runMessage msg a@(Knuckleduster attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> Knuckleduster <$> liftRunMessage msg attrs

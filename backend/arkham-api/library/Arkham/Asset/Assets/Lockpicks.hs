module Arkham.Asset.Assets.Lockpicks (lockpicks, lockpicksEffect, Lockpicks (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (withSkillTest)

newtype Lockpicks = Lockpicks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks :: AssetCard Lockpicks
lockpicks = asset Lockpicks Cards.lockpicks

instance HasAbilities Lockpicks where
  getAbilities (Lockpicks a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage Lockpicks where
  runMessage msg a@(Lockpicks attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      createCardEffect Cards.lockpicks (effectMetaTarget sid) attrs iid
      investigate sid iid (attrs.ability 1)
      pure a
    _ -> Lockpicks <$> liftRunMessage msg attrs

newtype LockpicksEffect = LockpicksEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicksEffect :: EffectArgs -> LockpicksEffect
lockpicksEffect = cardEffect LockpicksEffect Cards.lockpicks

instance HasModifiersFor LockpicksEffect where
  getModifiersFor (LockpicksEffect a) =
    modified_ a a.target [AddSkillValue #agility]

instance RunMessage LockpicksEffect where
  runMessage msg e@(LockpicksEffect attrs) = runQueueT $ case msg of
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    PassedThisSkillTestBy iid _ n | n < 2 -> do
      withSkillTest \sid -> do
        when (maybe False (isTarget sid) attrs.metaTarget) $ do
          let aid = fromJustNote "must be an asset" attrs.source.asset
          toDiscardBy iid attrs.source aid
          disable attrs
      pure e
    FailedThisSkillTest iid _ -> do
      withSkillTest \sid -> do
        when (maybe False (isTarget sid) attrs.metaTarget) $ do
          let aid = fromJustNote "must be an asset" attrs.source.asset
          toDiscardBy iid attrs.source aid
          disable attrs
      pure e
    _ -> LockpicksEffect <$> liftRunMessage msg attrs

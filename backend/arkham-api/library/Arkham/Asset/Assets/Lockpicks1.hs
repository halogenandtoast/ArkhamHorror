module Arkham.Asset.Assets.Lockpicks1 (lockpicks1, lockpicks1Effect, Lockpicks1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest
import Arkham.Investigate

newtype Lockpicks1 = Lockpicks1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks1 :: AssetCard Lockpicks1
lockpicks1 = assetWith Lockpicks1 Cards.lockpicks1 discardWhenNoUses

instance HasAbilities Lockpicks1 where
  getAbilities (Lockpicks1 a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage Lockpicks1 where
  runMessage msg a@(Lockpicks1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      createCardEffect Cards.lockpicks1 (effectMetaTarget sid) (attrs.ability 1) iid
      pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    _ -> Lockpicks1 <$> liftRunMessage msg attrs

newtype Lockpicks1Effect = Lockpicks1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks1Effect :: EffectArgs -> Lockpicks1Effect
lockpicks1Effect = cardEffect Lockpicks1Effect Cards.lockpicks1

instance HasModifiersFor Lockpicks1Effect where
  getModifiersFor (Lockpicks1Effect a) =
    modified_ a a.target [AddSkillValue #agility]

handleEffect :: ReverseQueue m => EffectAttrs -> m ()
handleEffect attrs = withSkillTest \sid -> do
  when (maybe False (isTarget sid) attrs.metaTarget) $ do
    let aid = fromJustNote "must be an asset" attrs.source.asset
    push $ SpendUses attrs.source (AssetTarget aid) Supply 1
    disable attrs

instance RunMessage Lockpicks1Effect where
  runMessage msg e@(Lockpicks1Effect attrs) = runQueueT $ case msg of
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    PassedThisSkillTestBy _ _ n | n < 2 -> do
      handleEffect attrs
      pure e
    FailedThisSkillTest _ _ -> do
      handleEffect attrs
      pure e
    _ -> Lockpicks1Effect <$> liftRunMessage msg attrs

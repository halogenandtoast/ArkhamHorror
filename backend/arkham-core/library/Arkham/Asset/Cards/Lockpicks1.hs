module Arkham.Asset.Cards.Lockpicks1 (
  lockpicks1,
  lockpicks1Effect,
  Lockpicks1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Investigate

newtype Lockpicks1 = Lockpicks1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks1 :: AssetCard Lockpicks1
lockpicks1 = asset Lockpicks1 Cards.lockpicks1

instance HasAbilities Lockpicks1 where
  getAbilities (Lockpicks1 a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage Lockpicks1 where
  runMessage msg a@(Lockpicks1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigation <- mkInvestigate iid (toAbilitySource attrs 1)
      pushAll [createCardEffect Cards.lockpicks1 Nothing attrs iid, toMessage investigation]
      pure a
    _ -> Lockpicks1 <$> runMessage msg attrs

newtype Lockpicks1Effect = Lockpicks1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks1Effect :: EffectArgs -> Lockpicks1Effect
lockpicks1Effect = cardEffect Lockpicks1Effect Cards.lockpicks1

instance HasModifiersFor Lockpicks1Effect where
  getModifiersFor target (Lockpicks1Effect a) | a.target `is` target = do
    pure $ toModifiers a [AddSkillValue #agility]
  getModifiersFor _ _ = pure []

instance RunMessage Lockpicks1Effect where
  runMessage msg e@(Lockpicks1Effect attrs) = case msg of
    SkillTestEnds _ _ -> do
      push $ disable attrs
      pure e
    PassedThisSkillTestBy _ _ n | n < 2 -> do
      let aid = fromJustNote "must be an asset" attrs.source.asset
      pushAll [SpendUses (AssetTarget aid) Supply 1, disable attrs]
      pure e
    FailedThisSkillTestBy _ _ n | n < 2 -> do
      let aid = fromJustNote "must be an asset" attrs.source.asset
      pushAll [SpendUses (AssetTarget aid) Supply 1, disable attrs]
      pure e
    _ -> Lockpicks1Effect <$> runMessage msg attrs

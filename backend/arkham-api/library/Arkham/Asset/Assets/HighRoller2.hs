module Arkham.Asset.Assets.HighRoller2 (highRoller2, highRoller2Effect, HighRoller2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype HighRoller2 = HighRoller2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRoller2 :: AssetCard HighRoller2
highRoller2 = asset HighRoller2 Cards.highRoller2

instance HasAbilities HighRoller2 where
  getAbilities (HighRoller2 a) =
    [ wantsSkillTest (YourSkillTest AnySkillTest)
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility (ResourceCost 3 <> exhaust a)
    ]

instance RunMessage HighRoller2 where
  runMessage msg a@(HighRoller2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs iid (AnySkillValue 2)
        createCardEffect Cards.highRoller2 (effectMetaTarget sid) attrs iid
      pure a
    _ -> HighRoller2 <$> liftRunMessage msg attrs

newtype HighRoller2Effect = HighRoller2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRoller2Effect :: EffectArgs -> HighRoller2Effect
highRoller2Effect = cardEffect HighRoller2Effect Cards.highRoller2

instance RunMessage HighRoller2Effect where
  runMessage msg e@(HighRoller2Effect attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ SkillTestInitiatorTarget {} _ _ | isTarget iid attrs.target -> do
      withSkillTest \sid -> do
        when (maybe False (isTarget sid) attrs.metaTarget) do
          gainResourcesIfCan iid attrs.source 3
          disable attrs
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> disableReturn e
    _ -> HighRoller2Effect <$> liftRunMessage msg attrs

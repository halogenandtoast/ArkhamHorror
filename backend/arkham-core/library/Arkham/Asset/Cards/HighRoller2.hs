module Arkham.Asset.Cards.HighRoller2 (
  highRoller2,
  highRoller2Effect,
  HighRoller2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher

newtype HighRoller2 = HighRoller2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRoller2 :: AssetCard HighRoller2
highRoller2 = asset HighRoller2 Cards.highRoller2

instance HasAbilities HighRoller2 where
  getAbilities (HighRoller2 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 3
        <> ExhaustCost (toTarget a)
    ]

instance RunMessage HighRoller2 where
  runMessage msg a@(HighRoller2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 2)
        , createCardEffect Cards.highRoller2 Nothing (toSource attrs) (InvestigatorTarget iid)
        ]
      pure a
    _ -> HighRoller2 <$> runMessage msg attrs

newtype HighRoller2Effect = HighRoller2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRoller2Effect :: EffectArgs -> HighRoller2Effect
highRoller2Effect = cardEffect HighRoller2Effect Cards.highRoller2

instance RunMessage HighRoller2Effect where
  runMessage msg e@(HighRoller2Effect attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest iid _ _ SkillTestInitiatorTarget {} _ _ | InvestigatorTarget iid == effectTarget -> do
      pushAll [TakeResources iid 3 (AbilitySource effectSource 1) False, DisableEffect effectId]
      pure e
    SkillTestEnds _ _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> HighRoller2Effect <$> runMessage msg attrs

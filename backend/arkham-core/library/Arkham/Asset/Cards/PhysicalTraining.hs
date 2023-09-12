module Arkham.Asset.Cards.PhysicalTraining (
  PhysicalTraining (..),
  physicalTraining,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype PhysicalTraining = PhysicalTraining AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining :: AssetCard PhysicalTraining
physicalTraining = asset PhysicalTraining Cards.physicalTraining

instance HasAbilities PhysicalTraining where
  getAbilities (PhysicalTraining a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ restrictedAbility a 2 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #willpower 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ skillTestModifier (toAbilitySource attrs 2) iid (SkillModifier #combat 1)
      pure a
    _ -> PhysicalTraining <$> runMessage msg attrs

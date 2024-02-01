module Arkham.Asset.Cards.HardKnocks (
  HardKnocks (..),
  hardKnocks,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype HardKnocks = HardKnocks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hardKnocks :: AssetCard HardKnocks
hardKnocks = asset HardKnocks Cards.hardKnocks

instance HasAbilities HardKnocks where
  getAbilities (HardKnocks a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ restrictedAbility a 2 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage HardKnocks where
  runMessage msg a@(HardKnocks attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #combat 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ skillTestModifier (toAbilitySource attrs 2) iid (SkillModifier #agility 1)
      pure a
    _ -> HardKnocks <$> runMessage msg attrs

module Arkham.Asset.Cards.BloodPact3 (bloodPact3, BloodPact3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype BloodPact3 = BloodPact3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodPact3 :: AssetCard BloodPact3
bloodPact3 = asset BloodPact3 Cards.bloodPact3

instance HasAbilities BloodPact3 where
  getAbilities (BloodPact3 x) =
    [ withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +3 {willpower} for this skill test. (Limit once per test.)"
        $ playerLimit PerTestOrAbility
        $ restrictedAbility x 1 ControlsThis (FastAbility $ ResourceCost 2)
    , withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +3 {combat} for this skill test. (Limit once per test.)"
        $ playerLimit PerTestOrAbility
        $ restrictedAbility x 2 ControlsThis (FastAbility $ ResourceCost 2)
    ]

instance RunMessage BloodPact3 where
  runMessage msg a@(BloodPact3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 3)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 3)
      pure a
    _ -> BloodPact3 <$> runMessage msg attrs

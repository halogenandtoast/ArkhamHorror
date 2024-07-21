module Arkham.Asset.Cards.BloodPact (bloodPact, BloodPact (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype BloodPact = BloodPact AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodPact :: AssetCard BloodPact
bloodPact = asset BloodPact Cards.bloodPact

instance HasAbilities BloodPact where
  getAbilities (BloodPact x) =
    [ withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +2 {willpower} for this skill test. (Limit once per test.)"
        $ playerLimit PerTestOrAbility
        $ restrictedAbility x 1 ControlsThis (FastAbility $ ResourceCost 2)
    , withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +2 {combat} for this skill test. (Limit once per test.)"
        $ playerLimit PerTestOrAbility
        $ restrictedAbility x 2 ControlsThis (FastAbility $ ResourceCost 2)
    ]

instance RunMessage BloodPact where
  runMessage msg a@(BloodPact attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 2)
      pure a
    _ -> BloodPact <$> runMessage msg attrs

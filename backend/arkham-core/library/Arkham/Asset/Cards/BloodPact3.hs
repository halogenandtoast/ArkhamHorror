module Arkham.Asset.Cards.BloodPact3
  ( bloodPact3
  , BloodPact3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype BloodPact3 = BloodPact3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodPact3 :: AssetCard BloodPact3
bloodPact3 = asset BloodPact3 Cards.bloodPact3

instance HasAbilities BloodPact3 where
  getAbilities (BloodPact3 x) =
    [ withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +3 {willpower} for this skill test. (Limit once per test.)"
      $ limitedAbility (PlayerLimit PerTestOrAbility 1)
      $ restrictedAbility x 1 ControlsThis (FastAbility $ ResourceCost 2)
    , withTooltip
        "{fast} Add 1 doom to Blood Pact: You get +3 {combat} for this skill test. (Limit once per test.)"
      $ limitedAbility (PlayerLimit PerTestOrAbility 1)
      $ restrictedAbility x 2 ControlsThis (FastAbility $ ResourceCost 2)
    ]

instance RunMessage BloodPact3 where
  runMessage msg a@(BloodPact3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    _ -> BloodPact3 <$> runMessage msg attrs

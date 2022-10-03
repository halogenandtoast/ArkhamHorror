module Arkham.Asset.Cards.HardKnocks2
  ( HardKnocks2(..)
  , hardKnocks2
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype HardKnocks2 = HardKnocks2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks2 :: AssetCard HardKnocks2
hardKnocks2 = asset HardKnocks2 Cards.hardKnocks2

instance HasAbilities HardKnocks2 where
  getAbilities (HardKnocks2 a) =
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

instance RunMessage HardKnocks2 where
  runMessage msg a@(HardKnocks2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      )
    _ -> HardKnocks2 <$> runMessage msg attrs

module Arkham.Asset.Cards.HardKnocks2 (HardKnocks2 (..), hardKnocks2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype HardKnocks2 = HardKnocks2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks2 :: AssetCard HardKnocks2
hardKnocks2 = asset HardKnocks2 Cards.hardKnocks2

instance HasAbilities HardKnocks2 where
  getAbilities (HardKnocks2 a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ controlledAbility a 2 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage HardKnocks2 where
  runMessage msg a@(HardKnocks2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #combat 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #agility 1)
      pure a
    _ -> HardKnocks2 <$> runMessage msg attrs

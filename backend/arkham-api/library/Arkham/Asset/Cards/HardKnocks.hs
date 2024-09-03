module Arkham.Asset.Cards.HardKnocks (HardKnocks (..), hardKnocks) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype HardKnocks = HardKnocks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks :: AssetCard HardKnocks
hardKnocks = asset HardKnocks Cards.hardKnocks

instance HasAbilities HardKnocks where
  getAbilities (HardKnocks a) =
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

instance RunMessage HardKnocks where
  runMessage msg a@(HardKnocks attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 1)
      pure a
    _ -> HardKnocks <$> runMessage msg attrs

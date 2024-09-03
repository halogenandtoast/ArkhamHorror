module Arkham.Asset.Cards.Hyperawareness2 (Hyperawareness2 (..), hyperawareness2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype Hyperawareness2 = Hyperawareness2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness2 :: AssetCard Hyperawareness2
hyperawareness2 = asset Hyperawareness2 Cards.hyperawareness2

instance HasAbilities Hyperawareness2 where
  getAbilities (Hyperawareness2 a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ controlledAbility a 2 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage Hyperawareness2 where
  runMessage msg a@(Hyperawareness2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #agility 1)
      pure a
    _ -> Hyperawareness2 <$> runMessage msg attrs

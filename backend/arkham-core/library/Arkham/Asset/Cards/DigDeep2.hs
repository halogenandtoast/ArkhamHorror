module Arkham.Asset.Cards.DigDeep2 (DigDeep2 (..), digDeep2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype DigDeep2 = DigDeep2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep2 :: AssetCard DigDeep2
digDeep2 = asset DigDeep2 Cards.digDeep2

instance HasAbilities DigDeep2 where
  getAbilities (DigDeep2 a) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    , withTooltip "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ controlledAbility a 2 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage DigDeep2 where
  runMessage msg a@(DigDeep2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        push $ skillTestModifier sid attrs iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        push $ skillTestModifier sid attrs iid (SkillModifier #agility 1)
      pure a
    _ -> DigDeep2 <$> runMessage msg attrs

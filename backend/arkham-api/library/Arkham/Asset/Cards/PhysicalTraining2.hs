module Arkham.Asset.Cards.PhysicalTraining2 (PhysicalTraining2 (..), physicalTraining2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype PhysicalTraining2 = PhysicalTraining2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining2 :: AssetCard PhysicalTraining2
physicalTraining2 = asset PhysicalTraining2 Cards.physicalTraining2

instance HasAbilities PhysicalTraining2 where
  getAbilities (PhysicalTraining2 a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ controlledAbility a 2 DuringAnySkillTest
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage PhysicalTraining2 where
  runMessage msg a@(PhysicalTraining2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #combat 1)
      pure a
    _ -> PhysicalTraining2 <$> runMessage msg attrs

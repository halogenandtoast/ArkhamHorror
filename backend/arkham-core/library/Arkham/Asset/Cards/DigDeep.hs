module Arkham.Asset.Cards.DigDeep (
  DigDeep (..),
  digDeep,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype DigDeep = DigDeep AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

digDeep :: AssetCard DigDeep
digDeep = asset DigDeep Cards.digDeep

instance HasAbilities DigDeep where
  getAbilities (DigDeep a) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ controlledAbility a 2 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    ]

instance RunMessage DigDeep where
  runMessage msg a@(DigDeep attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ skillTestModifier (toAbilitySource attrs 2) iid (SkillModifier #agility 1)
      pure a
    _ -> DigDeep <$> runMessage msg attrs

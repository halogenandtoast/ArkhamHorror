module Arkham.Asset.Cards.HigherEducation3 (higherEducation3, HigherEducation3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype HigherEducation3 = HigherEducation3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation3 :: AssetCard HigherEducation3
higherEducation3 = asset HigherEducation3 Cards.higherEducation3

instance HasAbilities HigherEducation3 where
  getAbilities (HigherEducation3 x) =
    [ withTooltip "{fast} Spend 1 resource: You get +2 {willpower} for this skill test."
        $ controlledAbility x 1 restriction (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +2 {intellect} for this skill test."
        $ controlledAbility x 2 restriction (FastAbility $ ResourceCost 1)
    ]
   where
    restriction = DuringAnySkillTest <> youExist (HandWith $ LengthIs $ atLeast 5)

instance RunMessage HigherEducation3 where
  runMessage msg a@(HigherEducation3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #intellect 2)
      pure a
    _ -> HigherEducation3 <$> runMessage msg attrs

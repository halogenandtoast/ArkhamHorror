module Arkham.Asset.Cards.HigherEducation (higherEducation, HigherEducation (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype HigherEducation = HigherEducation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

higherEducation :: AssetCard HigherEducation
higherEducation = asset HigherEducation Cards.higherEducation

instance HasAbilities HigherEducation where
  getAbilities (HigherEducation x) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ controlledAbility x 1 restriction (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
        $ controlledAbility x 2 restriction (FastAbility $ ResourceCost 1)
    ]
   where
    restriction = DuringAnySkillTest <> youExist (HandWith $ LengthIs $ atLeast 5)

instance RunMessage HigherEducation where
  runMessage msg a@(HigherEducation attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier attrs iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ skillTestModifier attrs iid (SkillModifier #intellect 1)
      pure a
    _ -> HigherEducation <$> runMessage msg attrs

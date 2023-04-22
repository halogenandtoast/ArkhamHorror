module Arkham.Asset.Cards.HigherEducation
  ( higherEducation
  , HigherEducation(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.GameValue
import Arkham.Matcher
import Arkham.SkillType

newtype HigherEducation = HigherEducation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation :: AssetCard HigherEducation
higherEducation = asset HigherEducation Cards.higherEducation

instance HasAbilities HigherEducation where
  getAbilities (HigherEducation x) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
      $ restrictedAbility x 1 restriction
      $ FastAbility
      $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
      $ restrictedAbility x 2 restriction
      $ FastAbility
      $ ResourceCost 1
    ]
   where
    restriction =
      ControlsThis <> DuringSkillTest AnySkillTest <> InvestigatorExists
        (You <> HandWith (LengthIs $ AtLeast $ Static 5))

instance RunMessage HigherEducation where
  runMessage msg a@(HigherEducation attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      )
    _ -> HigherEducation <$> runMessage msg attrs

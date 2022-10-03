module Arkham.Asset.Cards.HigherEducation3
  ( higherEducation3
  , HigherEducation3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype HigherEducation3 = HigherEducation3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation3 :: AssetCard HigherEducation3
higherEducation3 = asset HigherEducation3 Cards.higherEducation3

instance HasAbilities HigherEducation3 where
  getAbilities (HigherEducation3 x) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +2 {willpower} for this skill test."
      $ restrictedAbility x 1 restriction
      $ FastAbility
      $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +2 {intellect} for this skill test."
      $ restrictedAbility x 2 restriction
      $ FastAbility
      $ ResourceCost 1
    ]
   where
    restriction =
      ControlsThis <> DuringSkillTest AnySkillTest <> InvestigatorExists
        (You <> HandWith (LengthIs $ AtLeast $ Static 5))

instance RunMessage HigherEducation3 where
  runMessage msg a@(HigherEducation3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ SpendResources iid 1
      , skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 2)
      ]
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ pushAll
      [ SpendResources iid 1
      , skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 2)
      ]
    _ -> HigherEducation3 <$> runMessage msg attrs

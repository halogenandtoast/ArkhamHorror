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
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype HigherEducation3 = HigherEducation3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation3 :: AssetCard HigherEducation3
higherEducation3 = asset HigherEducation3 Cards.higherEducation3

instance HasAbilities HigherEducation3 where
  getAbilities (HigherEducation3 x) =
    [ restrictedAbility x idx restriction $ FastAbility $ ResourceCost 1
    | idx <- [1, 2]
    ]
   where
    restriction =
      OwnsThis <> DuringSkillTest AnySkillTest <> InvestigatorExists
        (You <> HandWith (LengthIs $ AtLeast $ Static 5))

instance AssetRunner env => RunMessage HigherEducation3 where
  runMessage msg a@(HigherEducation3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ SpendResources iid 1
      , skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 2)
      ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ pushAll
      [ SpendResources iid 1
      , skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 2)
      ]
    _ -> HigherEducation3 <$> runMessage msg attrs

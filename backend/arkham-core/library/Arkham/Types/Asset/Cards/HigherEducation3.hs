module Arkham.Types.Asset.Cards.HigherEducation3
  ( higherEducation3
  , HigherEducation3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype HigherEducation3 = HigherEducation3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation3 :: AssetCard HigherEducation3
higherEducation3 = asset HigherEducation3 Cards.higherEducation3

instance HasActions HigherEducation3 where
  getActions (HigherEducation3 x) =
    [ restrictedAbility x idx restriction $ FastAbility $ ResourceCost 1
    | idx <- [1, 2]
    ]
   where
    restriction =
      OwnsThis <> DuringSkillTest AnySkillTest <> InvestigatorExists
        (You <> HandWith (LengthIs $ AtLeast $ Static 5))

instance AssetRunner env => RunMessage env HigherEducation3 where
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

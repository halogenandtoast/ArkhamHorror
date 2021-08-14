module Arkham.Types.Asset.Cards.HigherEducation
  ( higherEducation
  , HigherEducation(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype HigherEducation = HigherEducation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation :: AssetCard HigherEducation
higherEducation = asset HigherEducation Cards.higherEducation

instance HasList HandCard env InvestigatorId => HasAbilities env HigherEducation where
  getAbilities iid (WhenSkillTest SkillWillpower) (HigherEducation a)
    | ownedBy a iid = do
      active <- (>= 5) . length <$> getHandOf iid
      pure [ mkAbility a 1 $ FastAbility $ ResourceCost 1 | active ]
  getAbilities iid (WhenSkillTest SkillIntellect) (HigherEducation a)
    | ownedBy a iid = do
      active <- (>= 5) . length <$> getHandOf iid
      pure [ mkAbility a 2 $ FastAbility $ ResourceCost 1 | active ]
  getAbilities _ _ _ = pure []

instance HasModifiersFor env HigherEducation

instance AssetRunner env => RunMessage env HigherEducation where
  runMessage msg a@(HigherEducation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ SpendResources iid 1
      , skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ pushAll
      [ SpendResources iid 1
      , skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      ]
    _ -> HigherEducation <$> runMessage msg attrs

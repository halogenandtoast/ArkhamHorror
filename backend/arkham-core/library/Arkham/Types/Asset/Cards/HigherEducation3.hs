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
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype HigherEducation3 = HigherEducation3 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation3 :: AssetCard HigherEducation3
higherEducation3 = asset HigherEducation3 Cards.higherEducation3

instance HasList HandCard env InvestigatorId => HasActions env HigherEducation3 where
  getActions iid (WhenSkillTest SkillWillpower) (HigherEducation3 a)
    | ownedBy a iid = do
      active <- (>= 5) . length <$> getHandOf iid
      pure
        [ UseAbility
            iid
            (mkAbility (toSource a) 1 (FastAbility $ ResourceCost 1))
        | active
        ]
  getActions iid (WhenSkillTest SkillIntellect) (HigherEducation3 a)
    | ownedBy a iid = do
      active <- (>= 5) . length <$> getHandOf iid
      pure
        [ UseAbility
            iid
            (mkAbility (toSource a) 2 (FastAbility $ ResourceCost 1))
        | active
        ]
  getActions _ _ _ = pure []

instance HasModifiersFor env HigherEducation3

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

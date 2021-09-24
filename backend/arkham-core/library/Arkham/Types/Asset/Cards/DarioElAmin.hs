module Arkham.Types.Asset.Cards.DarioElAmin
  ( darioElAmin
  , DarioElAmin(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype DarioElAmin = DarioElAmin AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darioElAmin :: AssetCard DarioElAmin
darioElAmin = asset DarioElAmin Cards.darioElAmin

instance HasCount ResourceCount env InvestigatorId => HasModifiersFor env DarioElAmin where
  getModifiersFor _ (InvestigatorTarget iid) (DarioElAmin attrs)
    | attrs `ownedBy` iid = do
      resources <- unResourceCount <$> getCount iid
      pure $ toModifiers attrs $ if resources >= 10
        then [SkillModifier SkillWillpower 1, SkillModifier SkillIntellect 1]
        else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities DarioElAmin where
  getAbilities (DarioElAmin attrs) =
    [ restrictedAbility
          attrs
          1
          (OwnsThis <> LocationExists (YourLocation <> LocationWithoutEnemies))
        $ ActionAbility Nothing
        $ ExhaustCost
        $ toTarget attrs
    ]

instance AssetRunner env => RunMessage env DarioElAmin where
  runMessage msg a@(DarioElAmin attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 2 False)
    _ -> DarioElAmin <$> runMessage msg attrs

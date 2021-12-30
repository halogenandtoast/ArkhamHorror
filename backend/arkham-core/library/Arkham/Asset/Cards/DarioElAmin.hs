module Arkham.Asset.Cards.DarioElAmin
  ( darioElAmin
  , DarioElAmin(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Query
import Arkham.SkillType
import Arkham.Target

newtype DarioElAmin = DarioElAmin AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darioElAmin :: AssetCard DarioElAmin
darioElAmin = ally DarioElAmin Cards.darioElAmin (2, 2)

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

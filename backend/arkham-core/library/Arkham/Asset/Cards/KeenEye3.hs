module Arkham.Asset.Cards.KeenEye3
  ( keenEye3
  , KeenEye3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype KeenEye3 = KeenEye3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye3 :: AssetCard KeenEye3
keenEye3 = asset KeenEye3 Cards.keenEye3

instance HasAbilities KeenEye3 where
  getAbilities (KeenEye3 a) =
    [ restrictedAbility a idx OwnsThis (FastAbility $ ResourceCost 2)
    | idx <- [1, 2]
    ]

instance AssetRunner env => RunMessage KeenEye3 where
  runMessage msg a@(KeenEye3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      )
    _ -> KeenEye3 <$> runMessage msg attrs

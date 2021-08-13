module Arkham.Types.Asset.Cards.KeenEye3
  ( keenEye3
  , KeenEye3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype KeenEye3 = KeenEye3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye3 :: AssetCard KeenEye3
keenEye3 = asset KeenEye3 Cards.keenEye3

instance HasActions KeenEye3 where
  getActions (KeenEye3 a) =
    [ restrictedAbility a idx OwnsThis (FastAbility $ ResourceCost 2)
    | idx <- [1, 2]
    ]

instance AssetRunner env => RunMessage env KeenEye3 where
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

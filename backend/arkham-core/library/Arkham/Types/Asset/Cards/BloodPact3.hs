module Arkham.Types.Asset.Cards.BloodPact3
  ( bloodPact3
  , BloodPact3(..)
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

newtype BloodPact3 = BloodPact3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodPact3 :: AssetCard BloodPact3
bloodPact3 = asset BloodPact3 Cards.bloodPact3

instance HasActions BloodPact3 where
  getActions (BloodPact3 x) =
    [ restrictedAbility x idx OwnsThis (FastAbility $ ResourceCost 2)
        & abilityLimitL
        .~ PlayerLimit PerTestOrAbility 1
    | idx <- [1 .. 2]
    ]

instance AssetRunner env => RunMessage env BloodPact3 where
  runMessage msg a@(BloodPact3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    _ -> BloodPact3 <$> runMessage msg attrs

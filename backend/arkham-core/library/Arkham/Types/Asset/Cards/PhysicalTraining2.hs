module Arkham.Types.Asset.Cards.PhysicalTraining2
  ( PhysicalTraining2(..)
  , physicalTraining2
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

newtype PhysicalTraining2 = PhysicalTraining2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining2 :: AssetCard PhysicalTraining2
physicalTraining2 = asset PhysicalTraining2 Cards.physicalTraining2

instance HasActions PhysicalTraining2 where
  getActions (PhysicalTraining2 a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance (AssetRunner env) => RunMessage env PhysicalTraining2 where
  runMessage msg a@(PhysicalTraining2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      )
    _ -> PhysicalTraining2 <$> runMessage msg attrs

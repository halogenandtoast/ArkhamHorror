module Arkham.Asset.Cards.PhysicalTraining2
  ( PhysicalTraining2(..)
  , physicalTraining2
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype PhysicalTraining2 = PhysicalTraining2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining2 :: AssetCard PhysicalTraining2
physicalTraining2 = asset PhysicalTraining2 Cards.physicalTraining2

instance HasAbilities PhysicalTraining2 where
  getAbilities (PhysicalTraining2 a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance (AssetRunner env) => RunMessage PhysicalTraining2 where
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

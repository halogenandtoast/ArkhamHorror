module Arkham.Asset.Cards.CrystallineElderSign3
  ( crystallineElderSign3
  , CrystallineElderSign3(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype CrystallineElderSign3 = CrystallineElderSign3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystallineElderSign3 :: AssetCard CrystallineElderSign3
crystallineElderSign3 = asset CrystallineElderSign3 Cards.crystallineElderSign3

instance HasModifiersFor CrystallineElderSign3 where
  getModifiersFor (InvestigatorTarget iid) (CrystallineElderSign3 a)
    | controlledBy a iid = pure $ toModifiers
      a
      [ SkillModifier skill 1
      | skill <- [SkillWillpower, SkillIntellect, SkillCombat, SkillAgility]
      ]
  getModifiersFor _ _ = pure []

instance RunMessage CrystallineElderSign3 where
  runMessage msg (CrystallineElderSign3 attrs) =
    CrystallineElderSign3 <$> runMessage msg attrs

module Arkham.Asset.Assets.CrystallineElderSign3 (
  crystallineElderSign3,
  CrystallineElderSign3 (..),
) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype CrystallineElderSign3 = CrystallineElderSign3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystallineElderSign3 :: AssetCard CrystallineElderSign3
crystallineElderSign3 = asset CrystallineElderSign3 Cards.crystallineElderSign3

instance HasModifiersFor CrystallineElderSign3 where
  getModifiersFor (CrystallineElderSign3 a) =
    controllerGets a [SkillModifier skill 1 | skill <- [#willpower, #intellect, #combat, #agility]]

instance RunMessage CrystallineElderSign3 where
  runMessage msg (CrystallineElderSign3 attrs) =
    CrystallineElderSign3 <$> runMessage msg attrs

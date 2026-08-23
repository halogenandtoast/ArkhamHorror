module Arkham.Asset.Assets.ChosenOfZburamoarteFightingTheHunger (chosenOfZburamoarteFightingTheHunger) where

import Arkham.Asset.Cards.ChildrenOfBlood qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ChosenOfZburamoarteFightingTheHunger = ChosenOfZburamoarteFightingTheHunger AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chosenOfZburamoarteFightingTheHunger :: AssetCard ChosenOfZburamoarteFightingTheHunger
chosenOfZburamoarteFightingTheHunger = asset ChosenOfZburamoarteFightingTheHunger Cards.chosenOfZburamoarteFightingTheHunger

instance RunMessage ChosenOfZburamoarteFightingTheHunger where
  runMessage msg (ChosenOfZburamoarteFightingTheHunger attrs) = ChosenOfZburamoarteFightingTheHunger <$> runMessage msg attrs

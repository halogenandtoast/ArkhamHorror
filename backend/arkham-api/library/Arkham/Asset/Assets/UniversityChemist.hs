module Arkham.Asset.Assets.UniversityChemist (universityChemist) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype UniversityChemist = UniversityChemist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universityChemist :: AssetCard UniversityChemist
universityChemist = ally UniversityChemist Cards.universityChemist (3, 1)

instance RunMessage UniversityChemist where
  runMessage msg (UniversityChemist attrs) = UniversityChemist <$> runMessage msg attrs

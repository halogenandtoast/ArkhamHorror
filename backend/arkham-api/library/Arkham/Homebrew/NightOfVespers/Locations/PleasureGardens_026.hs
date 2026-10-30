-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.PleasureGardens_026 (pleasureGardens_026) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype PleasureGardens_026 = PleasureGardens_026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pleasureGardens_026 :: LocationCard PleasureGardens_026
pleasureGardens_026 = location PleasureGardens_026 Cards.pleasureGardens_026 3 (PerPlayer 2)

instance RunMessage PleasureGardens_026 where
  runMessage msg (PleasureGardens_026 attrs) = PleasureGardens_026 <$> runMessage msg attrs

-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaKitchens_017 (trattoriaKitchens_017) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype TrattoriaKitchens_017 = TrattoriaKitchens_017 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trattoriaKitchens_017 :: LocationCard TrattoriaKitchens_017
trattoriaKitchens_017 = location TrattoriaKitchens_017 Cards.trattoriaKitchens_017 1 (PerPlayer 2)

instance RunMessage TrattoriaKitchens_017 where
  runMessage msg (TrattoriaKitchens_017 attrs) = TrattoriaKitchens_017 <$> runMessage msg attrs

-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaKitchens_018 (trattoriaKitchens_018) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype TrattoriaKitchens_018 = TrattoriaKitchens_018 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trattoriaKitchens_018 :: LocationCard TrattoriaKitchens_018
trattoriaKitchens_018 = location TrattoriaKitchens_018 Cards.trattoriaKitchens_018 1 (PerPlayer 2)

instance RunMessage TrattoriaKitchens_018 where
  runMessage msg (TrattoriaKitchens_018 attrs) = TrattoriaKitchens_018 <$> runMessage msg attrs

-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.HedgeMaze (hedgeMaze) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype HedgeMaze = HedgeMaze LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hedgeMaze :: LocationCard HedgeMaze
hedgeMaze = location HedgeMaze Cards.hedgeMaze 4 (PerPlayer 2)

instance RunMessage HedgeMaze where
  runMessage msg (HedgeMaze attrs) = HedgeMaze <$> runMessage msg attrs

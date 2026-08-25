module Arkham.Homebrew.CircusExMortis.Locations.CampOutskirtsQuietForNow (campOutskirtsQuietForNow) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

-- Printed text is passive: connection + fury-position aliasing are wired up
-- by the Act that places this location and the fury-bag helper, not here.
newtype CampOutskirtsQuietForNow = CampOutskirtsQuietForNow LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

campOutskirtsQuietForNow :: LocationCard CampOutskirtsQuietForNow
campOutskirtsQuietForNow =
  location CampOutskirtsQuietForNow Cards.campOutskirtsQuietForNow 3 (Static 3)

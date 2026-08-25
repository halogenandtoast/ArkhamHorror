module Arkham.Homebrew.CircusExMortis.Locations.CampOutskirtsGuardedClosely (campOutskirtsGuardedClosely) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

-- Printed text is passive: connection + fury-position aliasing are wired up
-- by the Act that places this location and the fury-bag helper, not here.
newtype CampOutskirtsGuardedClosely = CampOutskirtsGuardedClosely LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

campOutskirtsGuardedClosely :: LocationCard CampOutskirtsGuardedClosely
campOutskirtsGuardedClosely =
  location CampOutskirtsGuardedClosely Cards.campOutskirtsGuardedClosely 3 (Static 4)

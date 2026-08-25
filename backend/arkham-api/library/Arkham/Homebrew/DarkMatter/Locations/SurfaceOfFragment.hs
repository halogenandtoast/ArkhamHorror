module Arkham.Homebrew.DarkMatter.Locations.SurfaceOfFragment (surfaceOfFragment) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype SurfaceOfFragment = SurfaceOfFragment LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | "Cannot be flipped." — modelled by having no other side; the resign action
Shall Dry and Die grants it is proxied onto this location from that agenda.
-}
surfaceOfFragment :: LocationCard SurfaceOfFragment
surfaceOfFragment = symbolLabel $ location SurfaceOfFragment Cards.surfaceOfFragment 4 (Static 0)

instance RunMessage SurfaceOfFragment where
  runMessage msg (SurfaceOfFragment attrs) = SurfaceOfFragment <$> runMessage msg attrs

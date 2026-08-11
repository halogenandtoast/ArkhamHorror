module Arkham.Homebrew.DarkMatter.Locations.SurfaceOfFragment (surfaceOfFragment) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype SurfaceOfFragment = SurfaceOfFragment LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Cannot be flipped." — modelled by having no other side; the resign action
Shall Dry and Die grants it lives on that agenda.
-}
surfaceOfFragment :: LocationCard SurfaceOfFragment
surfaceOfFragment = location SurfaceOfFragment Cards.surfaceOfFragment 4 (Static 0)

instance RunMessage SurfaceOfFragment where
  runMessage msg (SurfaceOfFragment attrs) = SurfaceOfFragment <$> runMessage msg attrs

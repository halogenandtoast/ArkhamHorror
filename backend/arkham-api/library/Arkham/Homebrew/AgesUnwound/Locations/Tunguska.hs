module Arkham.Homebrew.AgesUnwound.Locations.Tunguska (tunguska) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Tunguska = Tunguska LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Tunguska/ (@:ages-unwound:119@). Shroud 4 and a fixed zero clues: nothing
to find and no printed text. /The Tunguska Event/ is the Task that gives the
place a purpose.
-}
tunguska :: LocationCard Tunguska
tunguska = symbolLabel $ location Tunguska Cards.tunguska 4 (Static 0)

instance RunMessage Tunguska where
  runMessage msg (Tunguska attrs) = Tunguska <$> runMessage msg attrs

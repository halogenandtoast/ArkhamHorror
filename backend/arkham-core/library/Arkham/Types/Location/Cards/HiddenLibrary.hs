module Arkham.Types.Location.Cards.HiddenLibrary
  ( hiddenLibrary
  , HiddenLibrary(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HiddenLibrary = HiddenLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenLibrary :: LocationCard HiddenLibrary
hiddenLibrary =
  location HiddenLibrary Cards.hiddenLibrary 0 (Static 0) NoSymbol []

instance HasModifiersFor env HiddenLibrary

instance HasAbilities HiddenLibrary where
  getAbilities (HiddenLibrary attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env HiddenLibrary where
  runMessage msg (HiddenLibrary attrs) = HiddenLibrary <$> runMessage msg attrs

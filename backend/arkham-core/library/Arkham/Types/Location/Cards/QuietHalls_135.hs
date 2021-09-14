module Arkham.Types.Location.Cards.QuietHalls_135
  ( quietHalls_135
  , QuietHalls_135(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype QuietHalls_135 = QuietHalls_135 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_135 :: LocationCard QuietHalls_135
quietHalls_135 =
  location QuietHalls_135 Cards.quietHalls_135 0 (Static 0) NoSymbol []

instance HasModifiersFor env QuietHalls_135

instance HasAbilities QuietHalls_135 where
  getAbilities (QuietHalls_135 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env QuietHalls_135 where
  runMessage msg (QuietHalls_135 attrs) =
    QuietHalls_135 <$> runMessage msg attrs

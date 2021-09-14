module Arkham.Types.Location.Cards.QuietHalls_131
  ( quietHalls_131
  , QuietHalls_131(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype QuietHalls_131 = QuietHalls_131 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls_131 :: LocationCard QuietHalls_131
quietHalls_131 =
  location QuietHalls_131 Cards.quietHalls_131 0 (Static 0) NoSymbol []

instance HasModifiersFor env QuietHalls_131

instance HasAbilities QuietHalls_131 where
  getAbilities (QuietHalls_131 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env QuietHalls_131 where
  runMessage msg (QuietHalls_131 attrs) =
    QuietHalls_131 <$> runMessage msg attrs

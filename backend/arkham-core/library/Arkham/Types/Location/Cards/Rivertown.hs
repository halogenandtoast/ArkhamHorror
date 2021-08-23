module Arkham.Types.Location.Cards.Rivertown where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (rivertown)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Rivertown = Rivertown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

rivertown :: LocationCard Rivertown
rivertown = location
  Rivertown
  Cards.rivertown
  1
  (PerPlayer 1)
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]

instance LocationRunner env => RunMessage env Rivertown where
  runMessage msg (Rivertown attrs) = Rivertown <$> runMessage msg attrs

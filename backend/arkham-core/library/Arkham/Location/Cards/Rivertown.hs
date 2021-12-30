module Arkham.Location.Cards.Rivertown where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (rivertown)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype Rivertown = Rivertown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

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

module Arkham.Types.Location.Cards.Rivertown where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (rivertown)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol

newtype Rivertown = Rivertown LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertown :: LocationId -> Rivertown
rivertown = Rivertown . baseAttrs
  Cards.rivertown
  1
  (PerPlayer 1)
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]

instance HasModifiersFor env Rivertown where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Rivertown where
  getActions i window (Rivertown attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Rivertown where
  runMessage msg (Rivertown attrs) = Rivertown <$> runMessage msg attrs

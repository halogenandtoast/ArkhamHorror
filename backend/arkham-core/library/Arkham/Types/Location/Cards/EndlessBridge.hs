module Arkham.Types.Location.Cards.EndlessBridge
  ( endlessBridge
  , EndlessBridge(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import Arkham.Types.Trait

newtype EndlessBridge = EndlessBridge LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessBridge :: LocationId -> EndlessBridge
endlessBridge lid = EndlessBridge $ baseAttrs
  lid
  "02326"
  (Name "Endless Bridge" Nothing)
  EncounterSet.LostInTimeAndSpace
  4
  (Static 2)
  Triangle
  [Square, Squiggle]
  [Otherworld, Extradimensional]

instance HasModifiersFor env EndlessBridge where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env EndlessBridge where
  getActions iid window (EndlessBridge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env EndlessBridge where
  runMessage msg (EndlessBridge attrs) = EndlessBridge <$> runMessage msg attrs

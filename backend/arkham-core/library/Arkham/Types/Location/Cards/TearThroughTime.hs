module Arkham.Types.Location.Cards.TearThroughTime
  ( tearThroughTime
  , TearThroughTime(..)
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

newtype TearThroughTime = TearThroughTime LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughTime :: LocationId -> TearThroughTime
tearThroughTime lid = TearThroughTime $ baseAttrs
  lid
  "02322"
  (Name "Tear Through Time" Nothing)
  EncounterSet.LostInTimeAndSpace
  2
  (PerPlayer 2)
  Moon
  [Circle, Plus, Squiggle]
  [Otherworld]

instance HasModifiersFor env TearThroughTime where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env TearThroughTime where
  getActions iid window (TearThroughTime attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TearThroughTime where
  runMessage msg (TearThroughTime attrs) =
    TearThroughTime <$> runMessage msg attrs

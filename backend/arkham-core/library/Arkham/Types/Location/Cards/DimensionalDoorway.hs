module Arkham.Types.Location.Cards.DimensionalDoorway
  ( dimensionalDoorway
  , DimensionalDoorway(..)
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

newtype DimensionalDoorway = DimensionalDoorway LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalDoorway :: LocationId -> DimensionalDoorway
dimensionalDoorway lid = DimensionalDoorway $ baseAttrs
  lid
  "02328"
  (Name "Dimensional Doorway" Nothing)
  EncounterSet.LostInTimeAndSpace
  2
  (PerPlayer 1)
  Squiggle
  [Triangle, Moon]
  [Otherworld, Extradimensional]

instance HasModifiersFor env DimensionalDoorway where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DimensionalDoorway where
  getActions iid window (DimensionalDoorway attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env DimensionalDoorway where
  runMessage msg (DimensionalDoorway attrs) =
    DimensionalDoorway <$> runMessage msg attrs

module Arkham.Types.Location.Cards.FacultyOfficesTheHourIsLate where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (facultyOfficesTheHourIsLate)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier

newtype FacultyOfficesTheHourIsLate = FacultyOfficesTheHourIsLate LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facultyOfficesTheHourIsLate :: LocationId -> FacultyOfficesTheHourIsLate
facultyOfficesTheHourIsLate = FacultyOfficesTheHourIsLate . baseAttrs
  Cards.facultyOfficesTheHourIsLate
  2
  (Static 0)
  T
  [Circle]

instance HasModifiersFor env FacultyOfficesTheHourIsLate where
  getModifiersFor _ target (FacultyOfficesTheHourIsLate attrs)
    | isTarget attrs target = pure
    $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FacultyOfficesTheHourIsLate where
  getActions i window (FacultyOfficesTheHourIsLate attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env FacultyOfficesTheHourIsLate where
  runMessage msg (FacultyOfficesTheHourIsLate attrs) =
    FacultyOfficesTheHourIsLate <$> runMessage msg attrs

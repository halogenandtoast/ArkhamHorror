module Arkham.Types.Location.Cards.FacultyOfficesTheHourIsLate where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype FacultyOfficesTheHourIsLate = FacultyOfficesTheHourIsLate LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facultyOfficesTheHourIsLate :: FacultyOfficesTheHourIsLate
facultyOfficesTheHourIsLate = FacultyOfficesTheHourIsLate $ baseAttrs
  "02055"
  (Name "Faculty Offices" $ Just "The House is Late")
  EncounterSet.ExtracurricularActivity
  2
  (Static 0)
  T
  [Circle]
  [Miskatonic]

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

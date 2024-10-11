module Arkham.Location.Cards.FacultyOfficesTheHourIsLate where

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (facultyOfficesTheHourIsLate)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Prelude

newtype FacultyOfficesTheHourIsLate = FacultyOfficesTheHourIsLate LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

facultyOfficesTheHourIsLate :: LocationCard FacultyOfficesTheHourIsLate
facultyOfficesTheHourIsLate =
  location FacultyOfficesTheHourIsLate Cards.facultyOfficesTheHourIsLate 2 (Static 0)

instance HasModifiersFor FacultyOfficesTheHourIsLate where
  getModifiersFor target (FacultyOfficesTheHourIsLate attrs) | isTarget attrs target = do
    toModifiers attrs [Blocked | not attrs.revealed]
  getModifiersFor _ _ = pure []

instance RunMessage FacultyOfficesTheHourIsLate where
  runMessage msg (FacultyOfficesTheHourIsLate attrs) =
    FacultyOfficesTheHourIsLate <$> runMessage msg attrs

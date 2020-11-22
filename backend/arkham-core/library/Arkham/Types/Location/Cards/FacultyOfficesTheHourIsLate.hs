{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.FacultyOfficesTheHourIsLate where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype FacultyOfficesTheHourIsLate = FacultyOfficesTheHourIsLate Attrs
  deriving newtype (Show, ToJSON, FromJSON)

facultyOfficesTheHourIsLate :: FacultyOfficesTheHourIsLate
facultyOfficesTheHourIsLate = FacultyOfficesTheHourIsLate $ baseAttrs
  "02055"
  "Faculty Offices"
  EncounterSet.ExtracurricularActivity
  2
  (Static 0)
  T
  [Circle]
  [Miskatonic]

instance HasModifiersFor env FacultyOfficesTheHourIsLate where
  getModifiersFor _ target (FacultyOfficesTheHourIsLate attrs)
    | isTarget attrs target = pure [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FacultyOfficesTheHourIsLate where
  getActions i window (FacultyOfficesTheHourIsLate attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env FacultyOfficesTheHourIsLate where
  runMessage msg (FacultyOfficesTheHourIsLate attrs) =
    FacultyOfficesTheHourIsLate <$> runMessage msg attrs

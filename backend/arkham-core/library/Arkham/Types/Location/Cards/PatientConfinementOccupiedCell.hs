module Arkham.Types.Location.Cards.PatientConfinementOccupiedCell
  ( patientConfinementOccupiedCell
  , PatientConfinementOccupiedCell(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype PatientConfinementOccupiedCell = PatientConfinementOccupiedCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementOccupiedCell :: LocationCard PatientConfinementOccupiedCell
patientConfinementOccupiedCell = location
  PatientConfinementOccupiedCell
  Cards.patientConfinementOccupiedCell
  5
  (Static 1)
  Moon
  [Squiggle]

instance HasAbilities PatientConfinementOccupiedCell where
  getAbilities (PatientConfinementOccupiedCell attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env PatientConfinementOccupiedCell where
  runMessage msg (PatientConfinementOccupiedCell attrs) =
    PatientConfinementOccupiedCell <$> runMessage msg attrs

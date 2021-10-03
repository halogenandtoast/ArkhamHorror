module Arkham.Types.Location.Cards.PatientConfinementDrearyCell
  ( patientConfinementDrearyCell
  , PatientConfinementDrearyCell(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype PatientConfinementDrearyCell = PatientConfinementDrearyCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDrearyCell :: LocationCard PatientConfinementDrearyCell
patientConfinementDrearyCell = location
  PatientConfinementDrearyCell
  Cards.patientConfinementDrearyCell
  3
  (Static 1)
  Moon
  [Squiggle]

instance HasAbilities PatientConfinementDrearyCell where
  getAbilities (PatientConfinementDrearyCell attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env PatientConfinementDrearyCell where
  runMessage msg (PatientConfinementDrearyCell attrs) =
    PatientConfinementDrearyCell <$> runMessage msg attrs

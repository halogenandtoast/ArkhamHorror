module Arkham.Types.Location.Cards.PatientConfinementDanielsCell
  ( patientConfinementDanielsCell
  , PatientConfinementDanielsCell(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype PatientConfinementDanielsCell = PatientConfinementDanielsCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDanielsCell :: LocationCard PatientConfinementDanielsCell
patientConfinementDanielsCell = location
  PatientConfinementDanielsCell
  Cards.patientConfinementDanielsCell
  2
  (Static 1)
  Moon
  [Squiggle]

instance HasAbilities PatientConfinementDanielsCell where
  getAbilities (PatientConfinementDanielsCell attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env PatientConfinementDanielsCell where
  runMessage msg (PatientConfinementDanielsCell attrs) =
    PatientConfinementDanielsCell <$> runMessage msg attrs

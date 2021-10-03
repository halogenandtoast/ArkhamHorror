module Arkham.Types.Location.Cards.PatientConfinementFamiliarCell
  ( patientConfinementFamiliarCell
  , PatientConfinementFamiliarCell(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype PatientConfinementFamiliarCell = PatientConfinementFamiliarCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementFamiliarCell :: LocationCard PatientConfinementFamiliarCell
patientConfinementFamiliarCell = location
  PatientConfinementFamiliarCell
  Cards.patientConfinementFamiliarCell
  2
  (Static 1)
  Moon
  [Squiggle]

instance HasAbilities PatientConfinementFamiliarCell where
  getAbilities (PatientConfinementFamiliarCell attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env PatientConfinementFamiliarCell where
  runMessage msg (PatientConfinementFamiliarCell attrs) =
    PatientConfinementFamiliarCell <$> runMessage msg attrs

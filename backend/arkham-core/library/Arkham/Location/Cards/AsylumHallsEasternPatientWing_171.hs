module Arkham.Location.Cards.AsylumHallsEasternPatientWing_171
  ( asylumHallsEasternPatientWing_171
  , AsylumHallsEasternPatientWing_171(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype AsylumHallsEasternPatientWing_171 = AsylumHallsEasternPatientWing_171 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsEasternPatientWing_171
  :: LocationCard AsylumHallsEasternPatientWing_171
asylumHallsEasternPatientWing_171 = location
  AsylumHallsEasternPatientWing_171
  Cards.asylumHallsEasternPatientWing_171
  2
  (PerPlayer 1)

instance HasAbilities AsylumHallsEasternPatientWing_171 where
  getAbilities (AsylumHallsEasternPatientWing_171 attrs) = getAbilities attrs

instance RunMessage AsylumHallsEasternPatientWing_171 where
  runMessage msg (AsylumHallsEasternPatientWing_171 attrs) =
    AsylumHallsEasternPatientWing_171 <$> runMessage msg attrs

module Arkham.Location.Cards.AsylumHallsEasternPatientWing_171
  ( asylumHallsEasternPatientWing_171
  , AsylumHallsEasternPatientWing_171(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype AsylumHallsEasternPatientWing_171 = AsylumHallsEasternPatientWing_171 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsEasternPatientWing_171
  :: LocationCard AsylumHallsEasternPatientWing_171
asylumHallsEasternPatientWing_171 = location
  AsylumHallsEasternPatientWing_171
  Cards.asylumHallsEasternPatientWing_171
  2
  (PerPlayer 1)
  Hourglass
  [Circle, Heart, Squiggle]

instance HasAbilities AsylumHallsEasternPatientWing_171 where
  getAbilities (AsylumHallsEasternPatientWing_171 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env AsylumHallsEasternPatientWing_171 where
  runMessage msg (AsylumHallsEasternPatientWing_171 attrs) =
    AsylumHallsEasternPatientWing_171 <$> runMessage msg attrs

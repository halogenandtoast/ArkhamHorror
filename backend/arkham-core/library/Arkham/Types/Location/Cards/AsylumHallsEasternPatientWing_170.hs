module Arkham.Types.Location.Cards.AsylumHallsEasternPatientWing_170
  ( asylumHallsEasternPatientWing_170
  , AsylumHallsEasternPatientWing_170(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype AsylumHallsEasternPatientWing_170 = AsylumHallsEasternPatientWing_170 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsEasternPatientWing_170
  :: LocationCard AsylumHallsEasternPatientWing_170
asylumHallsEasternPatientWing_170 = location
  AsylumHallsEasternPatientWing_170
  Cards.asylumHallsEasternPatientWing_170
  3
  (Static 0)
  Hourglass
  [Circle, Heart, Squiggle]

instance HasAbilities AsylumHallsEasternPatientWing_170 where
  getAbilities (AsylumHallsEasternPatientWing_170 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env AsylumHallsEasternPatientWing_170 where
  runMessage msg (AsylumHallsEasternPatientWing_170 attrs) =
    AsylumHallsEasternPatientWing_170 <$> runMessage msg attrs

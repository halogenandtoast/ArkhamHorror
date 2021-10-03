module Arkham.Types.Location.Cards.AsylumHallsWesternPatientWing_168
  ( asylumHallsWesternPatientWing_168
  , AsylumHallsWesternPatientWing_168(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype AsylumHallsWesternPatientWing_168 = AsylumHallsWesternPatientWing_168 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsWesternPatientWing_168
  :: LocationCard AsylumHallsWesternPatientWing_168
asylumHallsWesternPatientWing_168 = location
  AsylumHallsWesternPatientWing_168
  Cards.asylumHallsWesternPatientWing_168
  2
  (Static 0)
  Circle
  [Hourglass, Triangle, Diamond]

instance HasAbilities AsylumHallsWesternPatientWing_168 where
  getAbilities (AsylumHallsWesternPatientWing_168 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env AsylumHallsWesternPatientWing_168 where
  runMessage msg (AsylumHallsWesternPatientWing_168 attrs) =
    AsylumHallsWesternPatientWing_168 <$> runMessage msg attrs

module Arkham.Location.Cards.AsylumHallsWesternPatientWing_169 (
  asylumHallsWesternPatientWing_169,
  AsylumHallsWesternPatientWing_169 (..),
) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AsylumHallsWesternPatientWing_169 = AsylumHallsWesternPatientWing_169 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

asylumHallsWesternPatientWing_169
  :: LocationCard AsylumHallsWesternPatientWing_169
asylumHallsWesternPatientWing_169 =
  location AsylumHallsWesternPatientWing_169 Cards.asylumHallsWesternPatientWing_169 3 (PerPlayer 1)

instance HasModifiersFor AsylumHallsWesternPatientWing_169 where
  getModifiersFor (AsylumHallsWesternPatientWing_169 a) =
    whenRevealed a $ modifySelect a (enemyAt a) [HorrorDealt 1]

instance RunMessage AsylumHallsWesternPatientWing_169 where
  runMessage msg (AsylumHallsWesternPatientWing_169 attrs) =
    AsylumHallsWesternPatientWing_169 <$> runMessage msg attrs

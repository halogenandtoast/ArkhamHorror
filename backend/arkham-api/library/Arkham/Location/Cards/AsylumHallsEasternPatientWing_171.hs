module Arkham.Location.Cards.AsylumHallsEasternPatientWing_171 (asylumHallsEasternPatientWing_171) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Ability

newtype AsylumHallsEasternPatientWing_171 = AsylumHallsEasternPatientWing_171 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsEasternPatientWing_171
  :: LocationCard AsylumHallsEasternPatientWing_171
asylumHallsEasternPatientWing_171 =
  location AsylumHallsEasternPatientWing_171 Cards.asylumHallsEasternPatientWing_171 2 (PerPlayer 1)

instance HasAbilities AsylumHallsEasternPatientWing_171 where
  getAbilities (AsylumHallsEasternPatientWing_171 a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #after You

instance RunMessage AsylumHallsEasternPatientWing_171 where
  runMessage msg l@(AsylumHallsEasternPatientWing_171 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      gainResources iid (attrs.ability 1) 2
      pure l
    _ -> AsylumHallsEasternPatientWing_171 <$> liftRunMessage msg attrs

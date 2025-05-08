module Arkham.Location.Cards.PatientConfinementOccupiedCell (patientConfinementOccupiedCell) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype PatientConfinementOccupiedCell = PatientConfinementOccupiedCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementOccupiedCell :: LocationCard PatientConfinementOccupiedCell
patientConfinementOccupiedCell =
  locationWith
    PatientConfinementOccupiedCell
    Cards.patientConfinementOccupiedCell
    5
    (Static 1)
    (costToEnterUnrevealedL .~ ClueCost (Static 1))

instance HasAbilities PatientConfinementOccupiedCell where
  getAbilities (PatientConfinementOccupiedCell a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage PatientConfinementOccupiedCell where
  runMessage msg l@(PatientConfinementOccupiedCell attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember ReleasedADangerousPatient
      pure l
    _ -> PatientConfinementOccupiedCell <$> liftRunMessage msg attrs

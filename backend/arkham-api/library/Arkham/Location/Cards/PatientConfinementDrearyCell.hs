module Arkham.Location.Cards.PatientConfinementDrearyCell (patientConfinementDrearyCell) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype PatientConfinementDrearyCell = PatientConfinementDrearyCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDrearyCell :: LocationCard PatientConfinementDrearyCell
patientConfinementDrearyCell =
  locationWith
    PatientConfinementDrearyCell
    Cards.patientConfinementDrearyCell
    3
    (Static 1)
    (costToEnterUnrevealedL .~ ClueCost (Static 1))

instance HasAbilities PatientConfinementDrearyCell where
  getAbilities (PatientConfinementDrearyCell a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage PatientConfinementDrearyCell where
  runMessage msg l@(PatientConfinementDrearyCell attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember KnowTheGuardsPatrols
      pure l
    _ -> PatientConfinementDrearyCell <$> liftRunMessage msg attrs

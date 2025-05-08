module Arkham.Location.Cards.PatientConfinementFamiliarCell (patientConfinementFamiliarCell) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype PatientConfinementFamiliarCell = PatientConfinementFamiliarCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementFamiliarCell :: LocationCard PatientConfinementFamiliarCell
patientConfinementFamiliarCell =
  locationWith
    PatientConfinementFamiliarCell
    Cards.patientConfinementFamiliarCell
    2
    (Static 1)
    (costToEnterUnrevealedL .~ ClueCost (Static 1))

instance HasAbilities PatientConfinementFamiliarCell where
  getAbilities (PatientConfinementFamiliarCell a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage PatientConfinementFamiliarCell where
  runMessage msg l@(PatientConfinementFamiliarCell attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower
        $ InvestigatorFieldCalculation iid InvestigatorHorror
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember RecalledTheWayOut
      pure l
    _ -> PatientConfinementFamiliarCell <$> liftRunMessage msg attrs

module Arkham.Location.Cards.PatientConfinementFamiliarCell (
  patientConfinementFamiliarCell,
  PatientConfinementFamiliarCell (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
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
    (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost (Static 1)])

instance HasAbilities PatientConfinementFamiliarCell where
  getAbilities (PatientConfinementFamiliarCell attrs) =
    extendRevealed attrs [skillTestAbility $ restrictedAbility attrs 1 Here actionAbility]

instance RunMessage PatientConfinementFamiliarCell where
  runMessage msg l@(PatientConfinementFamiliarCell attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push
        $ beginSkillTest sid iid (attrs.ability 1) attrs #willpower
        $ InvestigatorFieldCalculation iid InvestigatorHorror
      pure l
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isAbilitySource attrs 1 source -> l <$ push (Remember RecalledTheWayOut)
    _ -> PatientConfinementFamiliarCell <$> runMessage msg attrs

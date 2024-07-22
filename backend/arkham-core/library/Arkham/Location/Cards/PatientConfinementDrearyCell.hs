module Arkham.Location.Cards.PatientConfinementDrearyCell (
  patientConfinementDrearyCell,
  PatientConfinementDrearyCell (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
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
    (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost (Static 1)])

instance HasAbilities PatientConfinementDrearyCell where
  getAbilities (PatientConfinementDrearyCell attrs) =
    extendRevealed attrs [restrictedAbility attrs 1 Here actionAbility]

instance RunMessage PatientConfinementDrearyCell where
  runMessage msg l@(PatientConfinementDrearyCell attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isAbilitySource attrs 1 source -> l <$ push (Remember KnowTheGuardsPatrols)
    _ -> PatientConfinementDrearyCell <$> runMessage msg attrs

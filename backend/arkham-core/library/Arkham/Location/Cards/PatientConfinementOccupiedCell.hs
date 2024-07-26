module Arkham.Location.Cards.PatientConfinementOccupiedCell (
  patientConfinementOccupiedCell,
  PatientConfinementOccupiedCell (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
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
    (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost (Static 1)])

instance HasAbilities PatientConfinementOccupiedCell where
  getAbilities (PatientConfinementOccupiedCell attrs) =
    withBaseAbilities
      attrs
      [ skillTestAbility $ restrictedAbility attrs 1 Here $ ActionAbility [] (ActionCost 1)
      | locationRevealed attrs
      ]

instance RunMessage PatientConfinementOccupiedCell where
  runMessage msg l@(PatientConfinementOccupiedCell attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ Remember ReleasedADangerousPatient
      pure l
    _ -> PatientConfinementOccupiedCell <$> runMessage msg attrs

module Arkham.Location.Cards.PatientConfinementOccupiedCell
  ( patientConfinementOccupiedCell
  , PatientConfinementOccupiedCell(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Target

newtype PatientConfinementOccupiedCell = PatientConfinementOccupiedCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementOccupiedCell :: LocationCard PatientConfinementOccupiedCell
patientConfinementOccupiedCell = locationWith
  PatientConfinementOccupiedCell
  Cards.patientConfinementOccupiedCell
  5
  (Static 1)
  (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost 1])

instance HasAbilities PatientConfinementOccupiedCell where
  getAbilities (PatientConfinementOccupiedCell attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing (ActionCost 1)
    | locationRevealed attrs
    ]

instance RunMessage PatientConfinementOccupiedCell where
  runMessage msg l@(PatientConfinementOccupiedCell attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push
        (beginSkillTest iid source (toTarget attrs) Nothing SkillCombat 2)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember ReleasedADangerousPatient)
    _ -> PatientConfinementOccupiedCell <$> runMessage msg attrs

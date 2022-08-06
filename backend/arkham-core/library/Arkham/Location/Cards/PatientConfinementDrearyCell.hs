module Arkham.Location.Cards.PatientConfinementDrearyCell
  ( patientConfinementDrearyCell
  , PatientConfinementDrearyCell(..)
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

newtype PatientConfinementDrearyCell = PatientConfinementDrearyCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDrearyCell :: LocationCard PatientConfinementDrearyCell
patientConfinementDrearyCell = locationWith
  PatientConfinementDrearyCell
  Cards.patientConfinementDrearyCell
  3
  (Static 1)
  (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost 1])

instance HasAbilities PatientConfinementDrearyCell where
  getAbilities (PatientConfinementDrearyCell attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing (ActionCost 1)
    | locationRevealed attrs
    ]

instance RunMessage PatientConfinementDrearyCell where
  runMessage msg l@(PatientConfinementDrearyCell attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 2)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember KnowTheGuardsPatrols)
    _ -> PatientConfinementDrearyCell <$> runMessage msg attrs

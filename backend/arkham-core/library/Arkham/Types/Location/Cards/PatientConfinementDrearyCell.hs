module Arkham.Types.Location.Cards.PatientConfinementDrearyCell
  ( patientConfinementDrearyCell
  , PatientConfinementDrearyCell(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype PatientConfinementDrearyCell = PatientConfinementDrearyCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDrearyCell :: LocationCard PatientConfinementDrearyCell
patientConfinementDrearyCell = locationWith
  PatientConfinementDrearyCell
  Cards.patientConfinementDrearyCell
  3
  (Static 1)
  Moon
  [Squiggle]
  (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost 1])

instance HasAbilities PatientConfinementDrearyCell where
  getAbilities (PatientConfinementDrearyCell attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing (ActionCost 1)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env PatientConfinementDrearyCell where
  runMessage msg l@(PatientConfinementDrearyCell attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 2)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember KnowTheGuardsPatrols)
    _ -> PatientConfinementDrearyCell <$> runMessage msg attrs

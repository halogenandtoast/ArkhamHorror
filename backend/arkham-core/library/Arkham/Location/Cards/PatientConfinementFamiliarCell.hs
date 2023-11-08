module Arkham.Location.Cards.PatientConfinementFamiliarCell (
  patientConfinementFamiliarCell,
  PatientConfinementFamiliarCell (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.SkillType

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
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility [] (ActionCost 1)
      | locationRevealed attrs
      ]

instance RunMessage PatientConfinementFamiliarCell where
  runMessage msg l@(PatientConfinementFamiliarCell attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      push $ beginSkillTest iid source (toTarget attrs) SkillWillpower horror
      pure l
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> l <$ push (Remember RecalledTheWayOut)
    _ -> PatientConfinementFamiliarCell <$> runMessage msg attrs

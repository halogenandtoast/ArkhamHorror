module Arkham.Types.Location.Cards.PatientConfinementFamiliarCell
  ( patientConfinementFamiliarCell
  , PatientConfinementFamiliarCell(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype PatientConfinementFamiliarCell = PatientConfinementFamiliarCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementFamiliarCell :: LocationCard PatientConfinementFamiliarCell
patientConfinementFamiliarCell = locationWith
  PatientConfinementFamiliarCell
  Cards.patientConfinementFamiliarCell
  2
  (Static 1)
  Moon
  [Squiggle]
  (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost 1])

instance HasAbilities PatientConfinementFamiliarCell where
  getAbilities (PatientConfinementFamiliarCell attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ActionAbility Nothing (ActionCost 1)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env PatientConfinementFamiliarCell where
  runMessage msg l@(PatientConfinementFamiliarCell attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      horror <- unHorrorCount <$> getCount iid
      push $ BeginSkillTest
        iid
        source
        (toTarget attrs)
        Nothing
        SkillWillpower
        horror
      pure l
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember RecalledTheWayOut)
    _ -> PatientConfinementFamiliarCell <$> runMessage msg attrs

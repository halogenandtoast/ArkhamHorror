module Arkham.Location.Cards.PatientConfinementDanielsCell (
  patientConfinementDanielsCell,
  PatientConfinementDanielsCell (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype PatientConfinementDanielsCell = PatientConfinementDanielsCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDanielsCell :: LocationCard PatientConfinementDanielsCell
patientConfinementDanielsCell =
  locationWith
    PatientConfinementDanielsCell
    Cards.patientConfinementDanielsCell
    2
    (Static 1)
    (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost (Static 1)])

instance HasAbilities PatientConfinementDanielsCell where
  getAbilities (PatientConfinementDanielsCell attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation
          Timing.After
          Anyone
          (LocationWithId $ toId attrs)
      | locationRevealed attrs
      ]

instance RunMessage PatientConfinementDanielsCell where
  runMessage msg l@(PatientConfinementDanielsCell attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      actIds <- select AnyAct
      l
        <$ pushAll
          (map (\aid -> AdvanceAct aid source AdvancedWithOther) actIds)
    _ -> PatientConfinementDanielsCell <$> runMessage msg attrs

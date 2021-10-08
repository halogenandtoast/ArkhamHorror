module Arkham.Types.Location.Cards.PatientConfinementDanielsCell
  ( patientConfinementDanielsCell
  , PatientConfinementDanielsCell(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Timing qualified as Timing

newtype PatientConfinementDanielsCell = PatientConfinementDanielsCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patientConfinementDanielsCell :: LocationCard PatientConfinementDanielsCell
patientConfinementDanielsCell = locationWith
  PatientConfinementDanielsCell
  Cards.patientConfinementDanielsCell
  2
  (Static 1)
  Moon
  [Squiggle]
  (costToEnterUnrevealedL .~ Costs [ActionCost 1, ClueCost 1])

instance HasAbilities PatientConfinementDanielsCell where
  getAbilities (PatientConfinementDanielsCell attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ RevealLocation
        Timing.After
        Anyone
        (LocationWithId $ toId attrs)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env PatientConfinementDanielsCell where
  runMessage msg l@(PatientConfinementDanielsCell attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      actIds <- getSetList ()
      l <$ pushAll (map (`AdvanceAct` source) actIds)
    _ -> PatientConfinementDanielsCell <$> runMessage msg attrs

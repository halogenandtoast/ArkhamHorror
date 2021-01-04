module Arkham.Types.Location.Cards.ForgottenMarsh where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ForgottenMarsh = ForgottenMarsh Attrs
  deriving newtype (Show, ToJSON, FromJSON)

forgottenMarsh :: ForgottenMarsh
forgottenMarsh = ForgottenMarsh $ baseAttrs
  "81013"
  (Name "Forgotten Marsh" Nothing)
  EncounterSet.CurseOfTheRougarou
  2
  (Static 0)
  Diamond
  [Moon, Square, Triangle, Hourglass]
  [Wilderness, Bayou]

instance HasModifiersFor env ForgottenMarsh where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ForgottenMarsh where
  getActions i window (ForgottenMarsh attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ForgottenMarsh where
  runMessage msg l@(ForgottenMarsh attrs@Attrs {..}) = case msg of
    Will (MoveTo iid lid)
      | iid `elem` locationInvestigators && lid /= locationId -> l
      <$ unshiftMessage (SpendResources iid 2)
    _ -> ForgottenMarsh <$> runMessage msg attrs

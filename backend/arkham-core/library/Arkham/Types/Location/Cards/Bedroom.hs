module Arkham.Types.Location.Cards.Bedroom where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Bedroom = Bedroom LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroom :: Bedroom
bedroom = Bedroom $ baseAttrs
  "50015"
  (Name "Bedroom" Nothing)
  EncounterSet.ReturnToTheGathering
  2
  (PerPlayer 1)
  Heart
  [T]
  mempty

instance HasModifiersFor env Bedroom where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Bedroom where
  getActions i window (Bedroom attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Bedroom where
  runMessage msg l@(Bedroom attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l <$ unshiftMessage (RandomDiscard iid)
    _ -> Bedroom <$> runMessage msg attrs

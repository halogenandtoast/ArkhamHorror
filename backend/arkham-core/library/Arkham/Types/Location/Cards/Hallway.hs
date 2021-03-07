module Arkham.Types.Location.Cards.Hallway where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Name

newtype Hallway = Hallway LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallway :: LocationId -> Hallway
hallway lid = Hallway $ baseAttrs
  lid
  "01112"
  (Name "Hallway" Nothing)
  EncounterSet.TheGathering
  1
  (Static 0)
  Square
  [Triangle, Plus, Diamond]
  mempty

instance HasModifiersFor env Hallway where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Hallway where
  getActions i window (Hallway attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Hallway where
  runMessage msg (Hallway attrs) = Hallway <$> runMessage msg attrs

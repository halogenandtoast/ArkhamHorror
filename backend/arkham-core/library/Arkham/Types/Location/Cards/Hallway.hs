module Arkham.Types.Location.Cards.Hallway where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Hallway = Hallway LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallway :: Hallway
hallway = Hallway $ baseAttrs
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

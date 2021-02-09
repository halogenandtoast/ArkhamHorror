module Arkham.Types.Location.Cards.HoleInTheWall where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holeInTheWall :: HoleInTheWall
holeInTheWall = HoleInTheWall $ baseAttrs
  "50017"
  (Name "Hallway" Nothing)
  EncounterSet.ReturnToTheGathering
  1
  (Static 0)
  Square
  [T, Triangle, Plus, Diamond]
  mempty

instance HasModifiersFor env HoleInTheWall where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HoleInTheWall where
  getActions i window (HoleInTheWall attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env HoleInTheWall where
  runMessage msg (HoleInTheWall attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessages
        [ PlaceLocationMatching (LocationWithTitle "Attic")
        , PlaceLocationMatching (LocationWithTitle "Cellar")
        , PlaceLocationMatching (LocationWithTitle "Parlor")
        ]
      HoleInTheWall <$> runMessage msg attrs
    _ -> HoleInTheWall <$> runMessage msg attrs

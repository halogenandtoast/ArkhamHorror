module Arkham.Types.Location.Cards.HoleInTheWall where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (holeInTheWall)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holeInTheWall :: LocationCard HoleInTheWall
holeInTheWall = location
  HoleInTheWall
  Cards.holeInTheWall
  1
  (Static 0)
  Square
  [T, Triangle, Plus, Diamond]

instance HasModifiersFor env HoleInTheWall

instance ActionRunner env => HasActions env HoleInTheWall where
  getActions i window (HoleInTheWall attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env HoleInTheWall where
  runMessage msg (HoleInTheWall attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      pushAll
        [ PlaceLocationMatching (LocationWithTitle "Attic")
        , PlaceLocationMatching (LocationWithTitle "Cellar")
        , PlaceLocationMatching (LocationWithTitle "Parlor")
        ]
      HoleInTheWall <$> runMessage msg attrs
    _ -> HoleInTheWall <$> runMessage msg attrs

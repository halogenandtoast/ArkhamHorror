module Arkham.Types.Location.Cards.HoleInTheWall where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (holeInTheWall)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holeInTheWall :: LocationId -> HoleInTheWall
holeInTheWall = HoleInTheWall . baseAttrs
  Cards.holeInTheWall
  1
  (Static 0)
  Square
  [T, Triangle, Plus, Diamond]

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

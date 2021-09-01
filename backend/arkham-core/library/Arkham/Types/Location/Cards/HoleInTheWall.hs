module Arkham.Types.Location.Cards.HoleInTheWall where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (holeInTheWall)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holeInTheWall :: LocationCard HoleInTheWall
holeInTheWall = location
  HoleInTheWall
  Cards.holeInTheWall
  1
  (Static 0)
  Square
  [T, Triangle, Plus, Diamond]

instance HasAbilities env HoleInTheWall where
  getAbilities i window (HoleInTheWall attrs) =
    withBaseAbilities i window attrs $ pure
      [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env HoleInTheWall where
  runMessage msg l@(HoleInTheWall attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> l <$ pushAll
      [ PlaceLocationMatching (LocationWithTitle "Attic")
      , PlaceLocationMatching (LocationWithTitle "Cellar")
      , PlaceLocationMatching (LocationWithTitle "Parlor")
      ]
    _ -> HoleInTheWall <$> runMessage msg attrs

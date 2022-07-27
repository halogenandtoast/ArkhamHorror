module Arkham.Location.Cards.Hallway where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (hallway)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Data
import Arkham.Location.Runner

newtype Hallway = Hallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hallway :: LocationCard Hallway
hallway =
  location Hallway Cards.hallway 1 (Static 0) Square [Triangle, Plus, Diamond]

instance HasLocationData Hallway where
  toLocationData = LocationData
    { locationSymbol = Square
    , locationConnectedSymbols = [Triangle, Plus, Diamond]
    , locationShroud = 1
    , locationClues = Static 0
    }

instance RunMessage Hallway where
  runMessage msg (Hallway attrs) = Hallway <$> runMessage msg attrs

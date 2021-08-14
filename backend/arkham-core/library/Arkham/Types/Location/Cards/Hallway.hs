module Arkham.Types.Location.Cards.Hallway where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (hallway)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype Hallway = Hallway LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallway :: LocationCard Hallway
hallway =
  location Hallway Cards.hallway 1 (Static 0) Square [Triangle, Plus, Diamond]

instance HasModifiersFor env Hallway

instance ActionRunner env => HasAbilities env Hallway where
  getAbilities i window (Hallway attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env Hallway where
  runMessage msg (Hallway attrs) = Hallway <$> runMessage msg attrs

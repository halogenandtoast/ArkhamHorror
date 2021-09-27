module Arkham.Types.Location.Cards.Hallway where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (hallway)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Hallway = Hallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hallway :: LocationCard Hallway
hallway =
  location Hallway Cards.hallway 1 (Static 0) Square [Triangle, Plus, Diamond]

instance (LocationRunner env) => RunMessage env Hallway where
  runMessage msg (Hallway attrs) = Hallway <$> runMessage msg attrs

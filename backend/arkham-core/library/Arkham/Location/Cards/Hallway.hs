module Arkham.Location.Cards.Hallway where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (hallway)
import Arkham.Location.Runner

newtype Hallway = Hallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

hallway :: LocationCard Hallway
hallway = location Hallway Cards.hallway 1 (Static 0)

instance RunMessage Hallway where
  runMessage msg (Hallway attrs) = Hallway <$> runMessage msg attrs

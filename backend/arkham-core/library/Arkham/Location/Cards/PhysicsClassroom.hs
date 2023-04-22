module Arkham.Location.Cards.PhysicsClassroom
  ( physicsClassroom
  , PhysicsClassroom(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message qualified as Msg

newtype PhysicsClassroom = PhysicsClassroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicsClassroom :: LocationCard PhysicsClassroom
physicsClassroom =
  location PhysicsClassroom Cards.physicsClassroom 4 (PerPlayer 1)

instance HasAbilities PhysicsClassroom where
  getAbilities (PhysicsClassroom attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage PhysicsClassroom where
  runMessage msg (PhysicsClassroom attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      PhysicsClassroom <$> runMessage msg (attrs & labelL .~ "physicsClassroom")
    _ -> PhysicsClassroom <$> runMessage msg attrs

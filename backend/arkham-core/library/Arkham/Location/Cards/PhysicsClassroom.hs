module Arkham.Location.Cards.PhysicsClassroom
  ( physicsClassroom
  , PhysicsClassroom(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

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
  runMessage msg (PhysicsClassroom attrs) =
    PhysicsClassroom <$> runMessage msg attrs

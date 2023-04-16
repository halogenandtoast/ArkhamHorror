module Arkham.Location.Cards.JoeMazurewiczsRoom
  ( joeMazurewiczsRoom
  , JoeMazurewiczsRoom(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype JoeMazurewiczsRoom = JoeMazurewiczsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeMazurewiczsRoom :: LocationCard JoeMazurewiczsRoom
joeMazurewiczsRoom =
  location JoeMazurewiczsRoom Cards.joeMazurewiczsRoom 3 (PerPlayer 1)

instance HasAbilities JoeMazurewiczsRoom where
  getAbilities (JoeMazurewiczsRoom attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage JoeMazurewiczsRoom where
  runMessage msg (JoeMazurewiczsRoom attrs) =
    JoeMazurewiczsRoom <$> runMessage msg attrs

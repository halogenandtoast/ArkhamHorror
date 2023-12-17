module Arkham.Location.Cards.ForbiddenLands (
  forbiddenLands,
  ForbiddenLands (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ForbiddenLands = ForbiddenLands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenLands :: LocationCard ForbiddenLands
forbiddenLands = location ForbiddenLands Cards.forbiddenLands 6 (Static 0)

instance HasAbilities ForbiddenLands where
  getAbilities (ForbiddenLands attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage ForbiddenLands where
  runMessage msg (ForbiddenLands attrs) =
    ForbiddenLands <$> runMessage msg attrs

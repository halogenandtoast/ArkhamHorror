module Arkham.Location.Cards.UnvisitedIsleDecayedWillow (
  unvisitedIsleDecayedWillow,
  UnvisitedIsleDecayedWillow (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UnvisitedIsleDecayedWillow = UnvisitedIsleDecayedWillow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDecayedWillow :: LocationCard UnvisitedIsleDecayedWillow
unvisitedIsleDecayedWillow = location UnvisitedIsleDecayedWillow Cards.unvisitedIsleDecayedWillow 4 (PerPlayer 2)

instance HasAbilities UnvisitedIsleDecayedWillow where
  getAbilities (UnvisitedIsleDecayedWillow attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage UnvisitedIsleDecayedWillow where
  runMessage msg (UnvisitedIsleDecayedWillow attrs) =
    UnvisitedIsleDecayedWillow <$> runMessage msg attrs

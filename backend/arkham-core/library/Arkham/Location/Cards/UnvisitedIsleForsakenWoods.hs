module Arkham.Location.Cards.UnvisitedIsleForsakenWoods (
  unvisitedIsleForsakenWoods,
  UnvisitedIsleForsakenWoods (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UnvisitedIsleForsakenWoods = UnvisitedIsleForsakenWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleForsakenWoods :: LocationCard UnvisitedIsleForsakenWoods
unvisitedIsleForsakenWoods = location UnvisitedIsleForsakenWoods Cards.unvisitedIsleForsakenWoods 2 (PerPlayer 2)

instance HasAbilities UnvisitedIsleForsakenWoods where
  getAbilities (UnvisitedIsleForsakenWoods attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage UnvisitedIsleForsakenWoods where
  runMessage msg (UnvisitedIsleForsakenWoods attrs) =
    UnvisitedIsleForsakenWoods <$> runMessage msg attrs

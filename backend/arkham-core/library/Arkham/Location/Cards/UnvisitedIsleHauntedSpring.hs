module Arkham.Location.Cards.UnvisitedIsleHauntedSpring (
  unvisitedIsleHauntedSpring,
  UnvisitedIsleHauntedSpring (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UnvisitedIsleHauntedSpring = UnvisitedIsleHauntedSpring LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleHauntedSpring :: LocationCard UnvisitedIsleHauntedSpring
unvisitedIsleHauntedSpring = location UnvisitedIsleHauntedSpring Cards.unvisitedIsleHauntedSpring 2 (PerPlayer 2)

instance HasAbilities UnvisitedIsleHauntedSpring where
  getAbilities (UnvisitedIsleHauntedSpring attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage UnvisitedIsleHauntedSpring where
  runMessage msg (UnvisitedIsleHauntedSpring attrs) =
    UnvisitedIsleHauntedSpring <$> runMessage msg attrs

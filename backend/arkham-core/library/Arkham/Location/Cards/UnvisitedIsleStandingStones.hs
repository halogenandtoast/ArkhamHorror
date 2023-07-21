module Arkham.Location.Cards.UnvisitedIsleStandingStones (
  unvisitedIsleStandingStones,
  UnvisitedIsleStandingStones (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UnvisitedIsleStandingStones = UnvisitedIsleStandingStones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleStandingStones :: LocationCard UnvisitedIsleStandingStones
unvisitedIsleStandingStones = location UnvisitedIsleStandingStones Cards.unvisitedIsleStandingStones 3 (PerPlayer 2)

instance HasAbilities UnvisitedIsleStandingStones where
  getAbilities (UnvisitedIsleStandingStones attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage UnvisitedIsleStandingStones where
  runMessage msg (UnvisitedIsleStandingStones attrs) =
    UnvisitedIsleStandingStones <$> runMessage msg attrs

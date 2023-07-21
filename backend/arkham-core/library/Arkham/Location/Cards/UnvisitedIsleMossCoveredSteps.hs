module Arkham.Location.Cards.UnvisitedIsleMossCoveredSteps (
  unvisitedIsleMossCoveredSteps,
  UnvisitedIsleMossCoveredSteps (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UnvisitedIsleMossCoveredSteps = UnvisitedIsleMossCoveredSteps LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleMossCoveredSteps :: LocationCard UnvisitedIsleMossCoveredSteps
unvisitedIsleMossCoveredSteps = location UnvisitedIsleMossCoveredSteps Cards.unvisitedIsleMossCoveredSteps 4 (PerPlayer 2)

instance HasAbilities UnvisitedIsleMossCoveredSteps where
  getAbilities (UnvisitedIsleMossCoveredSteps attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage UnvisitedIsleMossCoveredSteps where
  runMessage msg (UnvisitedIsleMossCoveredSteps attrs) =
    UnvisitedIsleMossCoveredSteps <$> runMessage msg attrs

module Arkham.Location.Cards.UnvisitedIsleMistyClearing (
  unvisitedIsleMistyClearing,
  UnvisitedIsleMistyClearing (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UnvisitedIsleMistyClearing = UnvisitedIsleMistyClearing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleMistyClearing :: LocationCard UnvisitedIsleMistyClearing
unvisitedIsleMistyClearing = location UnvisitedIsleMistyClearing Cards.unvisitedIsleMistyClearing 1 (PerPlayer 2)

instance HasAbilities UnvisitedIsleMistyClearing where
  getAbilities (UnvisitedIsleMistyClearing attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage UnvisitedIsleMistyClearing where
  runMessage msg (UnvisitedIsleMistyClearing attrs) =
    UnvisitedIsleMistyClearing <$> runMessage msg attrs

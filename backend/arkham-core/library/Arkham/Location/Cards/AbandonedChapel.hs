module Arkham.Location.Cards.AbandonedChapel (
  abandonedChapel,
  AbandonedChapel (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner

newtype AbandonedChapel = AbandonedChapel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedChapel :: LocationCard AbandonedChapel
abandonedChapel = location AbandonedChapel Cards.abandonedChapel 2 (PerPlayer 2)

instance HasAbilities AbandonedChapel where
  getAbilities (AbandonedChapel attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage AbandonedChapel where
  runMessage msg l@(AbandonedChapel attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.abandonedChapelSpectral
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> AbandonedChapel <$> runMessage msg attrs

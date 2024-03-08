module Arkham.Location.Cards.UpstairsHallway (
  upstairsHallway,
  UpstairsHallway (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UpstairsHallway = UpstairsHallway LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upstairsHallway :: LocationCard UpstairsHallway
upstairsHallway = location UpstairsHallway Cards.upstairsHallway 0 (Static 0)

instance HasModifiersFor UpstairsHallway where
  getModifiersFor target (UpstairsHallway attrs) | attrs `is` target = do
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities UpstairsHallway where
  getAbilities (UpstairsHallway attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage UpstairsHallway where
  runMessage msg (UpstairsHallway attrs) =
    UpstairsHallway <$> runMessage msg attrs

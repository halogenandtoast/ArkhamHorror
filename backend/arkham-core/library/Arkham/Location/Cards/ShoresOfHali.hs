module Arkham.Location.Cards.ShoresOfHali (
  shoresOfHali,
  ShoresOfHali (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype ShoresOfHali = ShoresOfHali LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

shoresOfHali :: LocationCard ShoresOfHali
shoresOfHali =
  locationWith
    ShoresOfHali
    Cards.shoresOfHali
    3
    (PerPlayer 2)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance RunMessage ShoresOfHali where
  runMessage msg (ShoresOfHali attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.songsThatTheHyadesShallSing
      pure . ShoresOfHali $ attrs & canBeFlippedL .~ False
    _ -> ShoresOfHali <$> runMessage msg attrs

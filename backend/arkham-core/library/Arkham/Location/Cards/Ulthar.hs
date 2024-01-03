module Arkham.Location.Cards.Ulthar (ulthar, Ulthar (..)) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Story.Cards qualified as Story

newtype Ulthar = Ulthar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ulthar :: LocationCard Ulthar
ulthar = locationWith Ulthar Cards.ulthar 1 (PerPlayer 1) (canBeFlippedL .~ True)

instance HasAbilities Ulthar where
  getAbilities (Ulthar attrs) = veiled attrs []

instance RunMessage Ulthar where
  runMessage msg (Ulthar attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.crypticSouls
      pure . Ulthar $ attrs & canBeFlippedL .~ False
    _ -> Ulthar <$> runMessage msg attrs

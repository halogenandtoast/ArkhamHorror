module Arkham.Location.Cards.Baharna (
  baharna,
  Baharna (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Story.Cards qualified as Story

newtype Baharna = Baharna LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

baharna :: LocationCard Baharna
baharna = location Baharna Cards.baharna 2 (PerPlayer 1)

instance HasAbilities Baharna where
  getAbilities (Baharna attrs) =
    veiled attrs []

instance RunMessage Baharna where
  runMessage msg (Baharna attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.waresOfBaharna
      pure . Baharna $ attrs & canBeFlippedL .~ False
    _ -> Baharna <$> runMessage msg attrs

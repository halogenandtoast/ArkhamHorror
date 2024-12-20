module Arkham.Location.Cards.Celephais (celephais) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Story.Cards qualified as Story

newtype Celephais = Celephais LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celephais :: LocationCard Celephais
celephais = location Celephais Cards.celephais 2 (PerPlayer 1)

instance HasAbilities Celephais where
  getAbilities (Celephais attrs) = veiled attrs []

instance RunMessage Celephais where
  runMessage msg (Celephais attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.adviceOfTheKing
      pure . Celephais $ attrs & canBeFlippedL .~ False
    _ -> Celephais <$> liftRunMessage msg attrs

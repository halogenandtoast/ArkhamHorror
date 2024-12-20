module Arkham.Location.Cards.Ulthar (ulthar) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Story.Cards qualified as Story

newtype Ulthar = Ulthar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ulthar :: LocationCard Ulthar
ulthar = location Ulthar Cards.ulthar 1 (PerPlayer 1)

instance HasAbilities Ulthar where
  getAbilities (Ulthar attrs) = veiled attrs []

instance RunMessage Ulthar where
  runMessage msg (Ulthar attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.crypticSouls
      pure . Ulthar $ attrs & canBeFlippedL .~ False
    _ -> Ulthar <$> liftRunMessage msg attrs

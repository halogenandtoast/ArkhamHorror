module Arkham.Location.Cards.ShoresOfHali (shoresOfHali) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype ShoresOfHali = ShoresOfHali LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shoresOfHali :: LocationCard ShoresOfHali
shoresOfHali =
  locationWith ShoresOfHali Cards.shoresOfHali 3 (PerPlayer 2)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance RunMessage ShoresOfHali where
  runMessage msg (ShoresOfHali attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.songsThatTheHyadesShallSing
      pure . ShoresOfHali $ attrs & canBeFlippedL .~ False
    _ -> ShoresOfHali <$> liftRunMessage msg attrs

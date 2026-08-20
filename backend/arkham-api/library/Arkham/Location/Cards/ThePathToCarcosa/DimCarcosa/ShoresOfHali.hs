module Arkham.Location.Cards.ThePathToCarcosa.DimCarcosa.ShoresOfHali (shoresOfHali) where

import Arkham.GameValue
import Arkham.Location.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Scenarios.ThePathToCarcosa.DimCarcosa.Helpers
import Arkham.Story.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Story

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

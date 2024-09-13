module Arkham.Location.Cards.SealedExit (
  sealedExit,
  SealedExit (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype SealedExit = SealedExit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealedExit :: LocationCard SealedExit
sealedExit = locationWith SealedExit Cards.sealedExit 5 (Static 0) connectsToAdjacent

instance HasAbilities SealedExit where
  getAbilities (SealedExit attrs) =
    extendRevealed attrs []

instance RunMessage SealedExit where
  runMessage msg (SealedExit attrs) = runQueueT $ case msg of
    _ -> SealedExit <$> liftRunMessage msg attrs

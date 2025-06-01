module Arkham.Location.Cards.SealedPassage (sealedPassage) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SealedPassage = SealedPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealedPassage :: LocationCard SealedPassage
sealedPassage = location SealedPassage Cards.sealedPassage 9 (Static 0)

instance HasAbilities SealedPassage where
  getAbilities (SealedPassage attrs) =
    extendRevealed attrs []

instance RunMessage SealedPassage where
  runMessage msg (SealedPassage attrs) = runQueueT $ case msg of
    _ -> SealedPassage <$> liftRunMessage msg attrs

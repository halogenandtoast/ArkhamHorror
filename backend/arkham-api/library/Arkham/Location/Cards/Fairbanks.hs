module Arkham.Location.Cards.Fairbanks (fairbanks) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Fairbanks = Fairbanks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fairbanks :: LocationCard Fairbanks
fairbanks = location Fairbanks Cards.fairbanks 0 (Static 0)

instance HasAbilities Fairbanks where
  getAbilities (Fairbanks attrs) =
    extendRevealed attrs []

instance RunMessage Fairbanks where
  runMessage msg (Fairbanks attrs) = runQueueT $ case msg of
    _ -> Fairbanks <$> liftRunMessage msg attrs

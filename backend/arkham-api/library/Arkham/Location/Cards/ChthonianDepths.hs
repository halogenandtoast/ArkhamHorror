module Arkham.Location.Cards.ChthonianDepths (chthonianDepths) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChthonianDepths = ChthonianDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chthonianDepths :: LocationCard ChthonianDepths
chthonianDepths = location ChthonianDepths Cards.chthonianDepths 0 (Static 0)

instance HasAbilities ChthonianDepths where
  getAbilities (ChthonianDepths attrs) =
    extendRevealed attrs []

instance RunMessage ChthonianDepths where
  runMessage msg (ChthonianDepths attrs) = runQueueT $ case msg of
    _ -> ChthonianDepths <$> liftRunMessage msg attrs

module Arkham.Location.Cards.TinMine (tinMine) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TinMine = TinMine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tinMine :: LocationCard TinMine
tinMine = symbolLabel $ location TinMine Cards.tinMine 0 (Static 0)

instance HasAbilities TinMine where
  getAbilities (TinMine a) =
    extendRevealed a []

instance RunMessage TinMine where
  runMessage msg (TinMine attrs) = runQueueT $ case msg of
    _ -> TinMine <$> liftRunMessage msg attrs

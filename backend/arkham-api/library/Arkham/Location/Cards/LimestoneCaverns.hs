module Arkham.Location.Cards.LimestoneCaverns (limestoneCaverns) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LimestoneCaverns = LimestoneCaverns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

limestoneCaverns :: LocationCard LimestoneCaverns
limestoneCaverns = location LimestoneCaverns Cards.limestoneCaverns 2 (Static 0)

instance HasAbilities LimestoneCaverns where
  getAbilities (LimestoneCaverns attrs) =
    extendRevealed attrs []

instance RunMessage LimestoneCaverns where
  runMessage msg (LimestoneCaverns attrs) = runQueueT $ case msg of
    _ -> LimestoneCaverns <$> liftRunMessage msg attrs

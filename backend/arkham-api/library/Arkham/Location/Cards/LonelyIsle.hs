module Arkham.Location.Cards.LonelyIsle
  ( lonelyIsle
  , LonelyIsle(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LonelyIsle = LonelyIsle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lonelyIsle :: LocationCard LonelyIsle
lonelyIsle = location LonelyIsle Cards.lonelyIsle 5 (Static 0)

instance HasAbilities LonelyIsle where
  getAbilities (LonelyIsle attrs) =
    extendRevealed attrs []

instance RunMessage LonelyIsle where
  runMessage msg (LonelyIsle attrs) = runQueueT $ case msg of
    _ -> LonelyIsle <$> liftRunMessage msg attrs

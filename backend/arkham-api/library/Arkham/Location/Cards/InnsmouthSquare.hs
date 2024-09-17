module Arkham.Location.Cards.InnsmouthSquare
  ( innsmouthSquare
  , InnsmouthSquare(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype InnsmouthSquare = InnsmouthSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthSquare :: LocationCard InnsmouthSquare
innsmouthSquare = location InnsmouthSquare Cards.innsmouthSquare 4 (PerPlayer 1)

instance HasAbilities InnsmouthSquare where
  getAbilities (InnsmouthSquare attrs) =
    extendRevealed attrs []

instance RunMessage InnsmouthSquare where
  runMessage msg (InnsmouthSquare attrs) = runQueueT $ case msg of
    _ -> InnsmouthSquare <$> liftRunMessage msg attrs

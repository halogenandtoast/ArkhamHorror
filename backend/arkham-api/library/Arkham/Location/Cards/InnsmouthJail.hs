module Arkham.Location.Cards.InnsmouthJail
  ( innsmouthJail
  , InnsmouthJail(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype InnsmouthJail = InnsmouthJail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthJail :: LocationCard InnsmouthJail
innsmouthJail = location InnsmouthJail Cards.innsmouthJail 4 (PerPlayer 1)

instance HasAbilities InnsmouthJail where
  getAbilities (InnsmouthJail attrs) =
    extendRevealed attrs []

instance RunMessage InnsmouthJail where
  runMessage msg (InnsmouthJail attrs) = runQueueT $ case msg of
    _ -> InnsmouthJail <$> liftRunMessage msg attrs

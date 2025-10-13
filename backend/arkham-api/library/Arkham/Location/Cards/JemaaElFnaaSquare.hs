module Arkham.Location.Cards.JemaaElFnaaSquare (jemaaElFnaaSquare) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype JemaaElFnaaSquare = JemaaElFnaaSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jemaaElFnaaSquare :: LocationCard JemaaElFnaaSquare
jemaaElFnaaSquare = symbolLabel $ location JemaaElFnaaSquare Cards.jemaaElFnaaSquare 3 (PerPlayer 3)

instance HasAbilities JemaaElFnaaSquare where
  getAbilities (JemaaElFnaaSquare attrs) =
    extendRevealed attrs []

instance RunMessage JemaaElFnaaSquare where
  runMessage msg (JemaaElFnaaSquare attrs) = runQueueT $ case msg of
    _ -> JemaaElFnaaSquare <$> liftRunMessage msg attrs

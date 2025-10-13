module Arkham.Location.Cards.JemaaElFnaaSquareAbandoned (jemaaElFnaaSquareAbandoned) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype JemaaElFnaaSquareAbandoned = JemaaElFnaaSquareAbandoned LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jemaaElFnaaSquareAbandoned :: LocationCard JemaaElFnaaSquareAbandoned
jemaaElFnaaSquareAbandoned = symbolLabel $ location JemaaElFnaaSquareAbandoned Cards.jemaaElFnaaSquareAbandoned 4 (Static 0)

instance HasAbilities JemaaElFnaaSquareAbandoned where
  getAbilities (JemaaElFnaaSquareAbandoned attrs) =
    extendRevealed attrs []

instance RunMessage JemaaElFnaaSquareAbandoned where
  runMessage msg (JemaaElFnaaSquareAbandoned attrs) = runQueueT $ case msg of
    _ -> JemaaElFnaaSquareAbandoned <$> liftRunMessage msg attrs

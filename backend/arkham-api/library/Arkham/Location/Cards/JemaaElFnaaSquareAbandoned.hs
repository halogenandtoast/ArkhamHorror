module Arkham.Location.Cards.JemaaElFnaaSquareAbandoned (jemaaElFnaaSquareAbandoned) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Modifier (ModifierType (..))

newtype JemaaElFnaaSquareAbandoned = JemaaElFnaaSquareAbandoned LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor JemaaElFnaaSquareAbandoned where
  getModifiersFor (JemaaElFnaaSquareAbandoned a) = hereGets a [IncreaseCostOf #asset 2]

jemaaElFnaaSquareAbandoned :: LocationCard JemaaElFnaaSquareAbandoned
jemaaElFnaaSquareAbandoned = symbolLabel $ location JemaaElFnaaSquareAbandoned Cards.jemaaElFnaaSquareAbandoned 4 (Static 0)

instance RunMessage JemaaElFnaaSquareAbandoned where
  runMessage msg (JemaaElFnaaSquareAbandoned attrs) =
    JemaaElFnaaSquareAbandoned <$> runMessage msg attrs

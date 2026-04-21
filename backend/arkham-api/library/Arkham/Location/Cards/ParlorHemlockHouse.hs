module Arkham.Location.Cards.ParlorHemlockHouse (parlorHemlockHouse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ParlorHemlockHouse = ParlorHemlockHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorHemlockHouse :: LocationCard ParlorHemlockHouse
parlorHemlockHouse = symbolLabel $ location ParlorHemlockHouse Cards.parlorHemlockHouse 4 (PerPlayer 2)

instance HasAbilities ParlorHemlockHouse where
  getAbilities (ParlorHemlockHouse a) =
    extendRevealed a []

instance RunMessage ParlorHemlockHouse where
  runMessage msg (ParlorHemlockHouse attrs) = runQueueT $ case msg of
    _ -> ParlorHemlockHouse <$> liftRunMessage msg attrs

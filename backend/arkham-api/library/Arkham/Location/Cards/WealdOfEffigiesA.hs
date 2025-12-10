module Arkham.Location.Cards.WealdOfEffigiesA (wealdOfEffigiesA) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WealdOfEffigiesA = WealdOfEffigiesA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wealdOfEffigiesA :: LocationCard WealdOfEffigiesA
wealdOfEffigiesA = symbolLabel $ locationWith WealdOfEffigiesA Cards.wealdOfEffigiesA 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WealdOfEffigiesA where
  getAbilities (WealdOfEffigiesA a) =
    extendRevealed a []

instance RunMessage WealdOfEffigiesA where
  runMessage msg (WealdOfEffigiesA attrs) = runQueueT $ case msg of
    _ -> WealdOfEffigiesA <$> liftRunMessage msg attrs

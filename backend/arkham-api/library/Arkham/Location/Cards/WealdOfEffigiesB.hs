module Arkham.Location.Cards.WealdOfEffigiesB (wealdOfEffigiesB) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WealdOfEffigiesB = WealdOfEffigiesB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wealdOfEffigiesB :: LocationCard WealdOfEffigiesB
wealdOfEffigiesB = symbolLabel $ locationWith WealdOfEffigiesB Cards.wealdOfEffigiesB 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WealdOfEffigiesB where
  getAbilities (WealdOfEffigiesB a) =
    extendRevealed a []

instance RunMessage WealdOfEffigiesB where
  runMessage msg (WealdOfEffigiesB attrs) = runQueueT $ case msg of
    _ -> WealdOfEffigiesB <$> liftRunMessage msg attrs

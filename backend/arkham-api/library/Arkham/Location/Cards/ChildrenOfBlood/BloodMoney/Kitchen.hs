module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Kitchen (kitchen) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = symbolLabel $ location Kitchen Cards.kitchen 4 (PerPlayer 1)

instance HasAbilities Kitchen where
  getAbilities (Kitchen a) = extendRevealed a []

instance RunMessage Kitchen where
  runMessage msg (Kitchen attrs) = runQueueT $ Kitchen <$> liftRunMessage msg attrs

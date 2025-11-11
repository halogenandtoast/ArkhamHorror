module Arkham.Location.Cards.PokerTable (pokerTable) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PokerTable = PokerTable LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pokerTable :: LocationCard PokerTable
pokerTable = symbolLabel $ location PokerTable Cards.pokerTable 0 (Static 0)

instance HasAbilities PokerTable where
  getAbilities (PokerTable attrs) =
    extendRevealed attrs []

instance RunMessage PokerTable where
  runMessage msg (PokerTable attrs) = runQueueT $ case msg of
    _ -> PokerTable <$> liftRunMessage msg attrs

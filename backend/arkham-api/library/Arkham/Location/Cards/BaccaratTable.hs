module Arkham.Location.Cards.BaccaratTable (baccaratTable) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BaccaratTable = BaccaratTable LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baccaratTable :: LocationCard BaccaratTable
baccaratTable = symbolLabel $ location BaccaratTable Cards.baccaratTable 0 (Static 0)

instance HasAbilities BaccaratTable where
  getAbilities (BaccaratTable attrs) =
    extendRevealed attrs []

instance RunMessage BaccaratTable where
  runMessage msg (BaccaratTable attrs) = runQueueT $ case msg of
    _ -> BaccaratTable <$> liftRunMessage msg attrs

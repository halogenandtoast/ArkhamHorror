module Arkham.Homebrew.CircusExMortis.Locations.StockCar (stockCar) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype StockCar = StockCar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stockCar :: LocationCard StockCar
stockCar = location StockCar Cards.stockCar 2 (Static 1)

instance HasAbilities StockCar where
  getAbilities (StockCar a) =
    extendRevealed1 a $ playerLimit PerRound $ restricted a 1 Here actionAbility

instance RunMessage StockCar where
  runMessage msg l@(StockCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 1
      pure l
    _ -> StockCar <$> liftRunMessage msg attrs

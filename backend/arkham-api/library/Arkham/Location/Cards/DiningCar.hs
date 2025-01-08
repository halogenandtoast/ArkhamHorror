module Arkham.Location.Cards.DiningCar (diningCar, DiningCar (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (diningCar)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DiningCar = DiningCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningCar :: LocationCard DiningCar
diningCar =
  locationWith DiningCar Cards.diningCar 2 (Static 0)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor DiningCar where
  getModifiersFor (DiningCar l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities DiningCar where
  getAbilities (DiningCar x) =
    extendRevealed1 x $ restricted x 1 Here $ forced $ RevealLocation #after You (be x)

instance RunMessage DiningCar where
  runMessage msg l@(DiningCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findAndDrawEncounterCard iid $ cardIs Enemies.grapplingHorror
      pure l
    _ -> DiningCar <$> liftRunMessage msg attrs

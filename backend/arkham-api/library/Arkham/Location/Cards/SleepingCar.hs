module Arkham.Location.Cards.SleepingCar (sleepingCar, SleepingCar (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (sleepingCar)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype SleepingCar = SleepingCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleepingCar :: LocationCard SleepingCar
sleepingCar =
  locationWith SleepingCar Cards.sleepingCar 4 (Static 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor SleepingCar where
  getModifiersFor (SleepingCar l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities SleepingCar where
  getAbilities (SleepingCar attrs) =
    extendRevealed1 attrs $ groupLimit PerGame $ restricted attrs 1 Here actionAbility

instance RunMessage SleepingCar where
  runMessage msg l@(SleepingCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesIfCan iid (attrs.ability 1) 3
      remember StolenAPassengersLuggage
      pure l
    _ -> SleepingCar <$> liftRunMessage msg attrs

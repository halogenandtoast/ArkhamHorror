module Arkham.Location.Cards.EngineCar_175 (engineCar_175, EngineCar_175 (..)) where

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (engineCar_175)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EngineCar_175 = EngineCar_175 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

engineCar_175 :: LocationCard EngineCar_175
engineCar_175 =
  locationWith EngineCar_175 Cards.engineCar_175 4 (PerPlayer 2)
    $ connectsToL
    .~ singleton LeftOf

instance HasModifiersFor EngineCar_175 where
  getModifiersFor (EngineCar_175 l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage EngineCar_175 where
  runMessage msg (EngineCar_175 attrs) = EngineCar_175 <$> runMessage msg attrs

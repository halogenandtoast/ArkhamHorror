module Arkham.Location.Cards.EngineCar_176 (engineCar_176, EngineCar_176 (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (engineCar_176)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EngineCar_176 = EngineCar_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineCar_176 :: LocationCard EngineCar_176
engineCar_176 =
  locationWith EngineCar_176 Cards.engineCar_176 2 (PerPlayer 2)
    $ connectsToL
    .~ singleton LeftOf

instance HasModifiersFor EngineCar_176 where
  getModifiersFor (EngineCar_176 l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities EngineCar_176 where
  getAbilities (EngineCar_176 x) =
    extendRevealed1 x
      $ restricted x 1 Here
      $ forced
      $ RevealLocation #after You (be x)

instance RunMessage EngineCar_176 where
  runMessage msg l@(EngineCar_176 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      findAndDrawEncounterCard iid $ cardIs Enemies.grapplingHorror
      pure l
    _ -> EngineCar_176 <$> liftRunMessage msg attrs

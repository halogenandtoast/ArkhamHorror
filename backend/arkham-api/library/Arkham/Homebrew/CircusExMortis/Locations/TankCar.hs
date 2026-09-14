module Arkham.Homebrew.CircusExMortis.Locations.TankCar (tankCar) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TankCar = TankCar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tankCar :: LocationCard TankCar
tankCar = location TankCar Cards.tankCar 1 (Static 1)

instance HasAbilities TankCar where
  getAbilities (TankCar a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithAnyClues)
      $ forced (TurnEnds #after You)

instance RunMessage TankCar where
  runMessage msg l@(TankCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 1
      pure l
    _ -> TankCar <$> liftRunMessage msg attrs

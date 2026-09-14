module Arkham.Homebrew.CircusExMortis.Locations.CoalHopperCar (coalHopperCar) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CoalHopperCar = CoalHopperCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coalHopperCar :: LocationCard CoalHopperCar
coalHopperCar = location CoalHopperCar Cards.coalHopperCar 3 (Static 2)

instance HasModifiersFor CoalHopperCar where
  getModifiersFor (CoalHopperCar a) = do
    noClues <- a.id <=~> LocationWithoutClues
    when noClues $ modifySelect a AnyAgenda [DoomThresholdModifier (-1)]

instance HasAbilities CoalHopperCar where
  getAbilities (CoalHopperCar a) = extendRevealed a []

instance RunMessage CoalHopperCar where
  runMessage msg (CoalHopperCar attrs) = runQueueT $ CoalHopperCar <$> liftRunMessage msg attrs

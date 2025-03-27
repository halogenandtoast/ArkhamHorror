module Arkham.Location.Cards.TempleOfTheElderThings (templeOfTheElderThings) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TempleOfTheElderThings = TempleOfTheElderThings LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheElderThings :: LocationCard TempleOfTheElderThings
templeOfTheElderThings =
  locationWith TempleOfTheElderThings Cards.templeOfTheElderThings 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities TempleOfTheElderThings where
  getAbilities (TempleOfTheElderThings attrs) =
    extendRevealed attrs []

instance RunMessage TempleOfTheElderThings where
  runMessage msg (TempleOfTheElderThings attrs) = runQueueT $ case msg of
    _ -> TempleOfTheElderThings <$> liftRunMessage msg attrs

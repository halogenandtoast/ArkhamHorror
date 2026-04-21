module Arkham.Location.Cards.WashroomHemlockHouse37 (washroomHemlockHouse37) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WashroomHemlockHouse37 = WashroomHemlockHouse37 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

washroomHemlockHouse37 :: LocationCard WashroomHemlockHouse37
washroomHemlockHouse37 = symbolLabel $ location WashroomHemlockHouse37 Cards.washroomHemlockHouse37 4 (PerPlayer 1)

instance HasAbilities WashroomHemlockHouse37 where
  getAbilities (WashroomHemlockHouse37 a) =
    extendRevealed a []

instance RunMessage WashroomHemlockHouse37 where
  runMessage msg (WashroomHemlockHouse37 attrs) = runQueueT $ case msg of
    _ -> WashroomHemlockHouse37 <$> liftRunMessage msg attrs

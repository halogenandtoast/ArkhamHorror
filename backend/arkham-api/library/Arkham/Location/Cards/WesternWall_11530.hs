module Arkham.Location.Cards.WesternWall_11530 (westernWall_11530) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WesternWall_11530 = WesternWall_11530 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernWall_11530 :: LocationCard WesternWall_11530
westernWall_11530 = location WesternWall_11530 Cards.westernWall_11530 0 (Static 2)

-- TODO: abilities

instance RunMessage WesternWall_11530 where
  runMessage msg (WesternWall_11530 attrs) = runQueueT $ WesternWall_11530 <$> liftRunMessage msg attrs

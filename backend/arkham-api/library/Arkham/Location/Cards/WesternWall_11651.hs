module Arkham.Location.Cards.WesternWall_11651 (westernWall_11651) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WesternWall_11651 = WesternWall_11651 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernWall_11651 :: LocationCard WesternWall_11651
westernWall_11651 = location WesternWall_11651 Cards.westernWall_11651 2 (Static 0)

-- TODO: abilities

instance RunMessage WesternWall_11651 where
  runMessage msg (WesternWall_11651 attrs) = runQueueT $ WesternWall_11651 <$> liftRunMessage msg attrs

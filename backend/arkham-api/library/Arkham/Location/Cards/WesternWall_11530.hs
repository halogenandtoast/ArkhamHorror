module Arkham.Location.Cards.WesternWall_11530 (westernWall_11530) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WesternWall_11530 = WesternWall_11530 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernWall_11530 :: LocationCard WesternWall_11530
westernWall_11530 = location WesternWall_11530 Cards.westernWall_11530 0 (Static 2)

instance HasAbilities WesternWall_11530 where
  getAbilities (WesternWall_11530 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here doubleActionAbility

instance RunMessage WesternWall_11530 where
  runMessage msg l@(WesternWall_11530 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> WesternWall_11530 <$> liftRunMessage msg attrs

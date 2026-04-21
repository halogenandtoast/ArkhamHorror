module Arkham.Location.Cards.LibraryHemlockHouse39 (libraryHemlockHouse39) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LibraryHemlockHouse39 = LibraryHemlockHouse39 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryHemlockHouse39 :: LocationCard LibraryHemlockHouse39
libraryHemlockHouse39 = locationWith LibraryHemlockHouse39 Cards.libraryHemlockHouse39 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities LibraryHemlockHouse39 where
  getAbilities (LibraryHemlockHouse39 a) =
    extendRevealed a []

instance RunMessage LibraryHemlockHouse39 where
  runMessage msg (LibraryHemlockHouse39 attrs) = runQueueT $ case msg of
    _ -> LibraryHemlockHouse39 <$> liftRunMessage msg attrs

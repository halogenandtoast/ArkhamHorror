module Arkham.Location.Cards.LibraryHemlockHouse40 (libraryHemlockHouse40) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LibraryHemlockHouse40 = LibraryHemlockHouse40 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryHemlockHouse40 :: LocationCard LibraryHemlockHouse40
libraryHemlockHouse40 = symbolLabel $ location LibraryHemlockHouse40 Cards.libraryHemlockHouse40 4 (PerPlayer 1)

instance HasAbilities LibraryHemlockHouse40 where
  getAbilities (LibraryHemlockHouse40 a) =
    extendRevealed a []

instance RunMessage LibraryHemlockHouse40 where
  runMessage msg (LibraryHemlockHouse40 attrs) = runQueueT $ case msg of
    _ -> LibraryHemlockHouse40 <$> liftRunMessage msg attrs

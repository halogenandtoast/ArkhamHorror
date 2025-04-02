module Arkham.Location.Cards.LibraryOfKos (libraryOfKos) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LibraryOfKos = LibraryOfKos LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryOfKos :: LocationCard LibraryOfKos
libraryOfKos = location LibraryOfKos Cards.libraryOfKos 5 (PerPlayer 1)

instance HasAbilities LibraryOfKos where
  getAbilities (LibraryOfKos attrs) =
    extendRevealed attrs []

instance RunMessage LibraryOfKos where
  runMessage msg (LibraryOfKos attrs) = runQueueT $ case msg of
    _ -> LibraryOfKos <$> liftRunMessage msg attrs

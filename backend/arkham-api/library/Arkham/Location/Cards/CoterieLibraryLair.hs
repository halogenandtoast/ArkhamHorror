module Arkham.Location.Cards.CoterieLibraryLair (coterieLibraryLair) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CoterieLibraryLair = CoterieLibraryLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieLibraryLair :: LocationCard CoterieLibraryLair
coterieLibraryLair = location CoterieLibraryLair Cards.coterieLibraryLair 0 (Static 0)

instance HasAbilities CoterieLibraryLair where
  getAbilities (CoterieLibraryLair a) =
    extendRevealed a []

instance RunMessage CoterieLibraryLair where
  runMessage msg (CoterieLibraryLair attrs) = runQueueT $ case msg of
    _ -> CoterieLibraryLair <$> liftRunMessage msg attrs

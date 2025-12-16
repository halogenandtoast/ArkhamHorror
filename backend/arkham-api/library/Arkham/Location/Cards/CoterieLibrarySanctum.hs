module Arkham.Location.Cards.CoterieLibrarySanctum (coterieLibrarySanctum) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CoterieLibrarySanctum = CoterieLibrarySanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieLibrarySanctum :: LocationCard CoterieLibrarySanctum
coterieLibrarySanctum = location CoterieLibrarySanctum Cards.coterieLibrarySanctum 0 (Static 0)

instance HasAbilities CoterieLibrarySanctum where
  getAbilities (CoterieLibrarySanctum a) =
    extendRevealed a []

instance RunMessage CoterieLibrarySanctum where
  runMessage msg (CoterieLibrarySanctum attrs) = runQueueT $ case msg of
    _ -> CoterieLibrarySanctum <$> liftRunMessage msg attrs

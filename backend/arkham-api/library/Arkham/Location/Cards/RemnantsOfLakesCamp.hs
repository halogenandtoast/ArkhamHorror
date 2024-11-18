module Arkham.Location.Cards.RemnantsOfLakesCamp (remnantsOfLakesCamp, RemnantsOfLakesCamp (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RemnantsOfLakesCamp = RemnantsOfLakesCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

remnantsOfLakesCamp :: LocationCard RemnantsOfLakesCamp
remnantsOfLakesCamp = symbolLabel $ location RemnantsOfLakesCamp Cards.remnantsOfLakesCamp 0 (Static 0)

instance HasAbilities RemnantsOfLakesCamp where
  getAbilities (RemnantsOfLakesCamp attrs) =
    extendRevealed attrs []

instance RunMessage RemnantsOfLakesCamp where
  runMessage msg (RemnantsOfLakesCamp attrs) = runQueueT $ case msg of
    _ -> RemnantsOfLakesCamp <$> liftRunMessage msg attrs

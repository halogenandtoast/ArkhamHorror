module Arkham.Location.Cards.SaadiansTombsAbandoned (saadiansTombsAbandoned) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SaadiansTombsAbandoned = SaadiansTombsAbandoned LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saadiansTombsAbandoned :: LocationCard SaadiansTombsAbandoned
saadiansTombsAbandoned = symbolLabel $ location SaadiansTombsAbandoned Cards.saadiansTombsAbandoned 5 (Static 0)

instance HasAbilities SaadiansTombsAbandoned where
  getAbilities (SaadiansTombsAbandoned attrs) =
    extendRevealed attrs []

instance RunMessage SaadiansTombsAbandoned where
  runMessage msg (SaadiansTombsAbandoned attrs) = runQueueT $ case msg of
    _ -> SaadiansTombsAbandoned <$> liftRunMessage msg attrs

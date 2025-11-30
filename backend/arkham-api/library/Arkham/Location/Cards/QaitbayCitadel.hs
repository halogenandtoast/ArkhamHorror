module Arkham.Location.Cards.QaitbayCitadel (qaitbayCitadel) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype QaitbayCitadel = QaitbayCitadel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

qaitbayCitadel :: LocationCard QaitbayCitadel
qaitbayCitadel = symbolLabel $ location QaitbayCitadel Cards.qaitbayCitadel 0 (Static 0)

instance HasAbilities QaitbayCitadel where
  getAbilities (QaitbayCitadel a) =
    extendRevealed a []

instance RunMessage QaitbayCitadel where
  runMessage msg (QaitbayCitadel attrs) = runQueueT $ case msg of
    _ -> QaitbayCitadel <$> liftRunMessage msg attrs

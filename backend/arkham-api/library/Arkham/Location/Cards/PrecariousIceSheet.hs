module Arkham.Location.Cards.PrecariousIceSheet (precariousIceSheet, PrecariousIceSheet (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PrecariousIceSheet = PrecariousIceSheet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

precariousIceSheet :: LocationCard PrecariousIceSheet
precariousIceSheet = symbolLabel $ location PrecariousIceSheet Cards.precariousIceSheet 0 (Static 0)

instance HasAbilities PrecariousIceSheet where
  getAbilities (PrecariousIceSheet attrs) =
    extendRevealed attrs []

instance RunMessage PrecariousIceSheet where
  runMessage msg (PrecariousIceSheet attrs) = runQueueT $ case msg of
    _ -> PrecariousIceSheet <$> liftRunMessage msg attrs

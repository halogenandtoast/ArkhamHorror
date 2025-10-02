module Arkham.Location.Cards.KensingtonGardens (kensingtonGardens) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype KensingtonGardens = KensingtonGardens LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kensingtonGardens :: LocationCard KensingtonGardens
kensingtonGardens = symbolLabel $ location KensingtonGardens Cards.kensingtonGardens 2 (PerPlayer 1)

instance HasAbilities KensingtonGardens where
  getAbilities (KensingtonGardens attrs) =
    extendRevealed attrs []

instance RunMessage KensingtonGardens where
  runMessage msg (KensingtonGardens attrs) = runQueueT $ case msg of
    _ -> KensingtonGardens <$> liftRunMessage msg attrs

module Arkham.Location.Cards.BancoDeLaProvincia (bancoDeLaProvincia) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BancoDeLaProvincia = BancoDeLaProvincia LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bancoDeLaProvincia :: LocationCard BancoDeLaProvincia
bancoDeLaProvincia = setLabel "d" $ location BancoDeLaProvincia Cards.bancoDeLaProvincia 3 (PerPlayer 1)

instance HasAbilities BancoDeLaProvincia where
  getAbilities (BancoDeLaProvincia attrs) =
    extendRevealed attrs []

instance RunMessage BancoDeLaProvincia where
  runMessage msg (BancoDeLaProvincia attrs) = runQueueT $ case msg of
    _ -> BancoDeLaProvincia <$> liftRunMessage msg attrs

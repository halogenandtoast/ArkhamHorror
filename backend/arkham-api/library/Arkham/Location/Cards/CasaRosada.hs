module Arkham.Location.Cards.CasaRosada (casaRosada) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CasaRosada = CasaRosada LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casaRosada :: LocationCard CasaRosada
casaRosada = setLabel "c" $ location CasaRosada Cards.casaRosada 4 (Static 2)

instance HasAbilities CasaRosada where
  getAbilities (CasaRosada attrs) =
    extendRevealed attrs []

instance RunMessage CasaRosada where
  runMessage msg (CasaRosada attrs) = runQueueT $ case msg of
    _ -> CasaRosada <$> liftRunMessage msg attrs

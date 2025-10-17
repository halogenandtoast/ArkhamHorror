module Arkham.Location.Cards.CementarioDeLaRecoleta (cementarioDeLaRecoleta) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CementarioDeLaRecoleta = CementarioDeLaRecoleta LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cementarioDeLaRecoleta :: LocationCard CementarioDeLaRecoleta
cementarioDeLaRecoleta = setLabel "h" $ location CementarioDeLaRecoleta Cards.cementarioDeLaRecoleta 2 (Static 3)

instance HasAbilities CementarioDeLaRecoleta where
  getAbilities (CementarioDeLaRecoleta attrs) =
    extendRevealed attrs []

instance RunMessage CementarioDeLaRecoleta where
  runMessage msg (CementarioDeLaRecoleta attrs) = runQueueT $ case msg of
    _ -> CementarioDeLaRecoleta <$> liftRunMessage msg attrs

module Arkham.Location.Cards.PearlEstateRuins (pearlEstateRuins) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PearlEstateRuins = PearlEstateRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pearlEstateRuins :: LocationCard PearlEstateRuins
pearlEstateRuins = symbolLabel $ locationWith PearlEstateRuins Cards.pearlEstateRuins 3 (Static 1) connectsToAdjacent

instance HasAbilities PearlEstateRuins where
  getAbilities (PearlEstateRuins a) =
    extendRevealed a []

instance RunMessage PearlEstateRuins where
  runMessage msg (PearlEstateRuins attrs) = runQueueT $ case msg of
    _ -> PearlEstateRuins <$> liftRunMessage msg attrs

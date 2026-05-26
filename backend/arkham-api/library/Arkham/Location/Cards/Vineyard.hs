module Arkham.Location.Cards.Vineyard (vineyard) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Vineyard = Vineyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vineyard :: LocationCard Vineyard
vineyard = symbolLabel $ locationWith Vineyard Cards.vineyard 0 (Static 0) connectsToAdjacent

instance HasAbilities Vineyard where
  getAbilities (Vineyard a) =
    extendRevealed a []

instance RunMessage Vineyard where
  runMessage msg (Vineyard attrs) = runQueueT $ case msg of
    _ -> Vineyard <$> liftRunMessage msg attrs

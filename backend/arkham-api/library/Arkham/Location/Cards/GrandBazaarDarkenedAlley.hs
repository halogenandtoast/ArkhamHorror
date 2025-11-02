module Arkham.Location.Cards.GrandBazaarDarkenedAlley (grandBazaarDarkenedAlley) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBazaarDarkenedAlley = GrandBazaarDarkenedAlley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarDarkenedAlley :: LocationCard GrandBazaarDarkenedAlley
grandBazaarDarkenedAlley = locationWith GrandBazaarDarkenedAlley Cards.grandBazaarDarkenedAlley 2 (Static 1) connectsToAdjacent

instance HasAbilities GrandBazaarDarkenedAlley where
  getAbilities (GrandBazaarDarkenedAlley attrs) =
    extendRevealed attrs []

instance RunMessage GrandBazaarDarkenedAlley where
  runMessage msg (GrandBazaarDarkenedAlley attrs) = runQueueT $ case msg of
    _ -> GrandBazaarDarkenedAlley <$> liftRunMessage msg attrs

module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterfrontWarehouseDawn (waterfrontWarehouseDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype WaterfrontWarehouseDawn = WaterfrontWarehouseDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterfrontWarehouseDawn :: LocationCard WaterfrontWarehouseDawn
waterfrontWarehouseDawn = symbolLabel $ location WaterfrontWarehouseDawn Cards.waterfrontWarehouseDawn 5 (PerPlayer 1)

instance HasAbilities WaterfrontWarehouseDawn where
  getAbilities (WaterfrontWarehouseDawn a) =
    extendRevealed a []

instance RunMessage WaterfrontWarehouseDawn where
  runMessage msg (WaterfrontWarehouseDawn attrs) = runQueueT $ case msg of
    _ -> WaterfrontWarehouseDawn <$> liftRunMessage msg attrs

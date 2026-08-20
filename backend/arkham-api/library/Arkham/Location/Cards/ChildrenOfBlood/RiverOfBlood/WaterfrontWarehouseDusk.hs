module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterfrontWarehouseDusk (waterfrontWarehouseDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype WaterfrontWarehouseDusk = WaterfrontWarehouseDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterfrontWarehouseDusk :: LocationCard WaterfrontWarehouseDusk
waterfrontWarehouseDusk = symbolLabel $ location WaterfrontWarehouseDusk Cards.waterfrontWarehouseDusk 5 (PerPlayer 1)

instance HasAbilities WaterfrontWarehouseDusk where
  getAbilities (WaterfrontWarehouseDusk a) =
    extendRevealed a []

instance RunMessage WaterfrontWarehouseDusk where
  runMessage msg (WaterfrontWarehouseDusk attrs) = runQueueT $ case msg of
    _ -> WaterfrontWarehouseDusk <$> liftRunMessage msg attrs

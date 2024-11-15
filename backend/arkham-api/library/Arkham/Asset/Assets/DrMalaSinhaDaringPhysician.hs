module Arkham.Asset.Assets.DrMalaSinhaDaringPhysician (
  drMalaSinhaDaringPhysician,
  DrMalaSinhaDaringPhysician (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DrMalaSinhaDaringPhysician = DrMalaSinhaDaringPhysician AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMalaSinhaDaringPhysician :: AssetCard DrMalaSinhaDaringPhysician
drMalaSinhaDaringPhysician = allyWith DrMalaSinhaDaringPhysician Cards.drMalaSinhaDaringPhysician (4, 2) noSlots

instance RunMessage DrMalaSinhaDaringPhysician where
  runMessage msg (DrMalaSinhaDaringPhysician attrs) = runQueueT $ case msg of
    _ -> DrMalaSinhaDaringPhysician <$> liftRunMessage msg attrs

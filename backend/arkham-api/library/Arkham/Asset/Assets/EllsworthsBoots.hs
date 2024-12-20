module Arkham.Asset.Assets.EllsworthsBoots (ellsworthsBoots) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype EllsworthsBoots = EllsworthsBoots AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ellsworthsBoots :: AssetCard EllsworthsBoots
ellsworthsBoots = asset EllsworthsBoots Cards.ellsworthsBoots

instance RunMessage EllsworthsBoots where
  runMessage msg (EllsworthsBoots attrs) = runQueueT $ case msg of
    _ -> EllsworthsBoots <$> liftRunMessage msg attrs

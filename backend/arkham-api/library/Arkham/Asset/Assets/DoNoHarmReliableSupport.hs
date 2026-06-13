module Arkham.Asset.Assets.DoNoHarmReliableSupport (doNoHarmCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DoNoHarmReliableSupport = DoNoHarmReliableSupport AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
doNoHarmCompleted :: AssetCard DoNoHarmReliableSupport
doNoHarmCompleted = asset DoNoHarmReliableSupport Cards.doNoHarmCompleted

instance RunMessage DoNoHarmReliableSupport where
  runMessage msg (DoNoHarmReliableSupport attrs) =
    runQueueT $ DoNoHarmReliableSupport <$> liftRunMessage msg attrs

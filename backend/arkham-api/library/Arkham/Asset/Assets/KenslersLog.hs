module Arkham.Asset.Assets.KenslersLog (kenslersLog) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype KenslersLog = KenslersLog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kenslersLog :: AssetCard KenslersLog
kenslersLog = asset KenslersLog Cards.kenslersLog

instance RunMessage KenslersLog where
  runMessage msg (KenslersLog attrs) = runQueueT $ case msg of
    _ -> KenslersLog <$> liftRunMessage msg attrs

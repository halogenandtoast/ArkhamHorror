module Arkham.Asset.Cards.DownTheRabbitHole
  ( downTheRabbitHole
  , DownTheRabbitHole(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DownTheRabbitHole = DownTheRabbitHole AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downTheRabbitHole :: AssetCard DownTheRabbitHole
downTheRabbitHole = asset DownTheRabbitHole Cards.downTheRabbitHole

instance RunMessage DownTheRabbitHole where
  runMessage msg (DownTheRabbitHole attrs) = runQueueT $ case msg of
    _ -> DownTheRabbitHole <$> liftRunMessage msg attrs

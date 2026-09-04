module Arkham.Asset.Assets.DownTheRabbitHole (downTheRabbitHole) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DownTheRabbitHole = DownTheRabbitHole AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downTheRabbitHole :: AssetCard DownTheRabbitHole
downTheRabbitHole = asset DownTheRabbitHole Cards.downTheRabbitHole

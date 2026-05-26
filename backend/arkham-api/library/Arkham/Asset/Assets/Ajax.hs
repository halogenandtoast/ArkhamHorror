module Arkham.Asset.Assets.Ajax (ajax) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype Ajax = Ajax AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ajax :: AssetCard Ajax
ajax = assetWith Ajax Cards.ajax (healthL ?~ 2)

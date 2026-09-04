module Arkham.Asset.Assets.UnderworldSupport ( underworldSupport,) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype UnderworldSupport = UnderworldSupport AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underworldSupport :: AssetCard UnderworldSupport
underworldSupport = asset UnderworldSupport Cards.underworldSupport

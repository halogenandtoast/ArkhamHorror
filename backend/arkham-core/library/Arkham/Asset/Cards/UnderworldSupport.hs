module Arkham.Asset.Cards.UnderworldSupport (
  underworldSupport,
  UnderworldSupport (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype UnderworldSupport = UnderworldSupport AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underworldSupport :: AssetCard UnderworldSupport
underworldSupport = asset UnderworldSupport Cards.underworldSupport

instance RunMessage UnderworldSupport where
  runMessage msg (UnderworldSupport attrs) = runQueueT $ case msg of
    _ -> UnderworldSupport <$> lift (runMessage msg attrs)

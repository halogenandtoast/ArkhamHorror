module Arkham.Asset.Assets.DanforthBrilliantStudent (
  danforthBrilliantStudent,
  DanforthBrilliantStudent (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DanforthBrilliantStudent = DanforthBrilliantStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danforthBrilliantStudent :: AssetCard DanforthBrilliantStudent
danforthBrilliantStudent = allyWith DanforthBrilliantStudent Cards.danforthBrilliantStudent (2, 4) noSlots

instance RunMessage DanforthBrilliantStudent where
  runMessage msg (DanforthBrilliantStudent attrs) = runQueueT $ case msg of
    _ -> DanforthBrilliantStudent <$> liftRunMessage msg attrs

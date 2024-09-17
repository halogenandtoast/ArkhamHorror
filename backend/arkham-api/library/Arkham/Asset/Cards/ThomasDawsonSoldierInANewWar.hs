module Arkham.Asset.Cards.ThomasDawsonSoldierInANewWar
  ( thomasDawsonSoldierInANewWar
  , ThomasDawsonSoldierInANewWar(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ThomasDawsonSoldierInANewWar = ThomasDawsonSoldierInANewWar AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasDawsonSoldierInANewWar :: AssetCard ThomasDawsonSoldierInANewWar
thomasDawsonSoldierInANewWar = asset ThomasDawsonSoldierInANewWar Cards.thomasDawsonSoldierInANewWar

instance RunMessage ThomasDawsonSoldierInANewWar where
  runMessage msg (ThomasDawsonSoldierInANewWar attrs) = runQueueT $ case msg of
    _ -> ThomasDawsonSoldierInANewWar <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.PlumbTheDepths (plumbTheDepths) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype PlumbTheDepths = PlumbTheDepths AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
plumbTheDepths :: AssetCard PlumbTheDepths
plumbTheDepths = asset PlumbTheDepths Cards.plumbTheDepths

instance RunMessage PlumbTheDepths where
  runMessage msg (PlumbTheDepths attrs) = runQueueT $ PlumbTheDepths <$> liftRunMessage msg attrs

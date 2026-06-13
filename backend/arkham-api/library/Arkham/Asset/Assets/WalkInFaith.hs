module Arkham.Asset.Assets.WalkInFaith (walkInFaith) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype WalkInFaith = WalkInFaith AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: Task progression — once per scenario the investigator may attempt their
-- Task objective; marking/erasing progress and the completion rewards are
-- prompted by individual scenarios.
walkInFaith :: AssetCard WalkInFaith
walkInFaith = asset WalkInFaith Cards.walkInFaith

instance RunMessage WalkInFaith where
  runMessage msg (WalkInFaith attrs) = runQueueT $ WalkInFaith <$> liftRunMessage msg attrs

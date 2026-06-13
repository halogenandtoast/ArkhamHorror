module Arkham.Asset.Assets.WalkInFaithFilledWithSpirit (walkInFaithCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype WalkInFaithFilledWithSpirit = WalkInFaithFilledWithSpirit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
walkInFaithCompleted :: AssetCard WalkInFaithFilledWithSpirit
walkInFaithCompleted = asset WalkInFaithFilledWithSpirit Cards.walkInFaithCompleted

instance RunMessage WalkInFaithFilledWithSpirit where
  runMessage msg (WalkInFaithFilledWithSpirit attrs) =
    runQueueT $ WalkInFaithFilledWithSpirit <$> liftRunMessage msg attrs

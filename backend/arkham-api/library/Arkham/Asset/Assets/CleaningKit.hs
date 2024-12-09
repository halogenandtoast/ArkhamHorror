module Arkham.Asset.Assets.CleaningKit (cleaningKit, CleaningKit (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype CleaningKit = CleaningKit AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleaningKit :: AssetCard CleaningKit
cleaningKit = asset CleaningKit Cards.cleaningKit

instance HasModifiersFor CleaningKit where
  getModifiersFor (CleaningKit a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      modifySelectWhen
        a
        (a.use Supply > 0)
        (not_ (AssetWithId a.id) <> assetControlledBy iid)
        [ProvidesUses Supply (toSource a), ProvidesProxyUses Supply Ammo (toSource a)]

instance RunMessage CleaningKit where
  runMessage msg (CleaningKit attrs) = runQueueT $ case msg of
    _ -> CleaningKit <$> liftRunMessage msg attrs

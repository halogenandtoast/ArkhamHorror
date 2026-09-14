module Arkham.Asset.Assets.CleaningKit (cleaningKit) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype CleaningKit = CleaningKit AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleaningKit :: AssetCard CleaningKit
cleaningKit = asset CleaningKit Cards.cleaningKit

instance HasModifiersFor CleaningKit where
  getModifiersFor (CleaningKit a) = for_ a.controller \iid ->
    modifySelectWhen
      a
      (a.use Supply > 0)
      (not_ (AssetWithId a.id) <> assetControlledBy iid)
      [ProvidesUses Supply (toSource a), ProvidesProxyUses Supply Ammo (toSource a)]

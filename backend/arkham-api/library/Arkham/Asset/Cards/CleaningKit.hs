module Arkham.Asset.Cards.CleaningKit (cleaningKit, CleaningKit (..)) where

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
  getModifiersFor (AssetTarget aid) (CleaningKit a) | a.id /= aid = maybeModified a do
    iid <- hoistMaybe a.controller
    liftGuardM $ aid <=~> assetControlledBy iid
    guard $ a.use Supply > 0
    pure [ProvidesUses Supply (toSource a), ProvidesProxyUses Supply Ammo (toSource a)]
  getModifiersFor _ _ = pure []

instance RunMessage CleaningKit where
  runMessage msg (CleaningKit attrs) = runQueueT $ case msg of
    _ -> CleaningKit <$> liftRunMessage msg attrs

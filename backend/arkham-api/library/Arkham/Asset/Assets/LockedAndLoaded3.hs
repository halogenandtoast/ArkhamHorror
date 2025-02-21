module Arkham.Asset.Assets.LockedAndLoaded3 (lockedAndLoaded3) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype LockedAndLoaded3 = LockedAndLoaded3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedAndLoaded3 :: AssetCard LockedAndLoaded3
lockedAndLoaded3 = asset LockedAndLoaded3 Cards.lockedAndLoaded3

instance HasModifiersFor LockedAndLoaded3 where
  getModifiersFor (LockedAndLoaded3 a) = for_ a.controller \iid -> do
    modifySelect
      a
      (assetControlledBy iid <> AssetWithUseType Ammo <> #firearm)
      [AdditionalStartingUses 1]

instance RunMessage LockedAndLoaded3 where
  runMessage msg (LockedAndLoaded3 attrs) = LockedAndLoaded3 <$> runMessage msg attrs

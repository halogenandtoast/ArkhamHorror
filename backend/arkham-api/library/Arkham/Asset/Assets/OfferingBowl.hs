module Arkham.Asset.Assets.OfferingBowl (offeringBowl) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher

newtype OfferingBowl = OfferingBowl AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

offeringBowl :: AssetCard OfferingBowl
offeringBowl = assetWith OfferingBowl Cards.offeringBowl discardWhenNoUses

instance HasAbilities OfferingBowl where
  getAbilities (OfferingBowl a) =
    [controlled a 1 (thisExists a AssetReady) $ freeTrigger (assetUseCost a Offering 1 <> exhaust a)]

instance RunMessage OfferingBowl where
  runMessage msg a@(OfferingBowl attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      gainResources iid (attrs.ability 1) 2
      pure a
    _ -> OfferingBowl <$> liftRunMessage msg attrs

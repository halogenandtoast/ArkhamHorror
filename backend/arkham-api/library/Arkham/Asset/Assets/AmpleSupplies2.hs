module Arkham.Asset.Assets.AmpleSupplies2 (ampleSupplies2) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Matcher

newtype AmpleSupplies2 = AmpleSupplies2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ampleSupplies2 :: AssetCard AmpleSupplies2
ampleSupplies2 = asset AmpleSupplies2 Cards.ampleSupplies2

instance HasAbilities AmpleSupplies2 where
  getAbilities (AmpleSupplies2 x) = 
    [ restricted x 1 ControlsThis $ triggered (DiscardedFromHand #after You #any #any) (exhaust x)
    , controlled x 2 (thisExists x $ AssetWithUses Supply) $ FastAbility (discardCost x)
    ]

instance RunMessage AmpleSupplies2 where
  runMessage msg a@(AmpleSupplies2 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      addUses (attrs.ability 1) attrs Supply 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      moveTokens (attrs.ability 2) attrs (ResourceTarget iid) Supply (attrs.use Supply)
      pure a
    _ -> AmpleSupplies2 <$> liftRunMessage msg attrs

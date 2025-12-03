module Arkham.Asset.Assets.DivingSuit (divingSuit) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype DivingSuit = DivingSuit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

divingSuit :: AssetCard DivingSuit
divingSuit = assetWith DivingSuit Cards.divingSuit (healthL ?~ 3)

instance HasAbilities DivingSuit where
  getAbilities (DivingSuit a) =
    [ controlled_ a 1 $ forced $ PlacedCounter #when You AnySource #damage (atLeast 1)
    , controlled_ a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage DivingSuit where
  runMessage msg a@(DivingSuit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ReassignDamage (toSource iid) (toTarget attrs) 1
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      pure a
    _ -> DivingSuit <$> liftRunMessage msg attrs

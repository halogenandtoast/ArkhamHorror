module Arkham.Asset.Assets.DivingSuit (divingSuit) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype DivingSuit = DivingSuit AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

divingSuit :: AssetCard DivingSuit
divingSuit = assetWith DivingSuit Cards.divingSuit (healthL ?~ 3)

instance HasModifiersFor DivingSuit where
  getModifiersFor (DivingSuit a) = modifySelf a [NonDirectDamageMustBeAssignToThisN 1]

instance HasAbilities DivingSuit where
  getAbilities (DivingSuit a) =
    [ controlled_ a 1 $ forced $ InvestigatorWouldTakeDamage #when You AnySource IsNonDirectDamage
    , controlled_ a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage DivingSuit where
  runMessage msg a@(DivingSuit attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      -- handled by modifier
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      pure a
    _ -> DivingSuit <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.DivingSuit (divingSuit, DivingSuit (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype DivingSuit = DivingSuit AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

divingSuit :: AssetCard DivingSuit
divingSuit = asset DivingSuit Cards.divingSuit

instance HasModifiersFor DivingSuit where
  getModifiersFor (DivingSuit a) = modifySelf a [NonDirectDamageMustBeAssignToThisN 1]

instance HasAbilities DivingSuit where
  getAbilities (DivingSuit a) =
    [ restricted a 1 ControlsThis
        $ forced
        $ InvestigatorWouldTakeDamage #when You AnySource IsNonDirectDamage
    , restricted a 2 ControlsThis
        $ forced
        $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage DivingSuit where
  runMessage msg a@(DivingSuit attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      -- handled by modifier
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> DivingSuit <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.Fame (fame) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Matcher

newtype Fame = Fame AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fame :: AssetCard Fame
fame = assetWith Fame Cards.fame discardWhenNoUses

instance HasAbilities Fame where
  getAbilities (Fame a) =
    [ controlled a 1 (exists (EnemyAt YourLocation) <> youExist can.gain.resources)
        $ parleyAction
        $ assetUseCost a Renown 1
    ]

instance RunMessage Fame where
  runMessage msg a@(Fame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      pure a
    _ -> Fame <$> liftRunMessage msg attrs

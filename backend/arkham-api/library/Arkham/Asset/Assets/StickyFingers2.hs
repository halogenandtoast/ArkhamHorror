module Arkham.Asset.Assets.StickyFingers2 (stickyFingers2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Matcher

newtype StickyFingers2 = StickyFingers2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickyFingers2 :: AssetCard StickyFingers2
stickyFingers2 = asset StickyFingers2 Cards.stickyFingers2

instance HasAbilities StickyFingers2 where
  getAbilities (StickyFingers2 a) =
    [ controlled a 1 (youExist can.gain.resources)
        $ triggered (EnemyEvadedSuccessfully #after You AnySource AnyEnemy) (exhaust a)
    ]

instance RunMessage StickyFingers2 where
  runMessage msg a@(StickyFingers2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> StickyFingers2 <$> liftRunMessage msg attrs

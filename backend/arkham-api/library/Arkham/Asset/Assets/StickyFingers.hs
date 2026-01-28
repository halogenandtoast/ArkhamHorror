module Arkham.Asset.Assets.StickyFingers (stickyFingers) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Matcher

newtype StickyFingers = StickyFingers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickyFingers :: AssetCard StickyFingers
stickyFingers = asset StickyFingers Cards.stickyFingers

instance HasAbilities StickyFingers where
  getAbilities (StickyFingers a) =
    [ controlled a 1 (youExist can.gain.resources)
        $ triggered (EnemyEvadedSuccessfully #after You AnySource AnyEnemy) (exhaust a)
    ]

instance RunMessage StickyFingers where
  runMessage msg a@(StickyFingers attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> StickyFingers <$> liftRunMessage msg attrs

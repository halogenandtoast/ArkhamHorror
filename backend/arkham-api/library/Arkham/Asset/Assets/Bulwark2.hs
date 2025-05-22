module Arkham.Asset.Assets.Bulwark2 (bulwark2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Bulwark2 = Bulwark2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bulwark2 :: AssetCard Bulwark2
bulwark2 = asset Bulwark2 Cards.bulwark2

instance HasAbilities Bulwark2 where
  getAbilities (Bulwark2 a) =
    [ restricted a 1 ControlsThis
        $ ConstantReaction
          "Trigger sealed keyword again"
          (EnemyDefeated #after You ByAny AnyEnemy)
          (SealCost $ oneOf [#eldersign, #"+1"])
    , restricted a 2 ControlsThis
        $ triggered
          (EnemyWouldAttack #when (affectsOthers $ colocatedWithMatch You) AnyEnemyAttack AnyEnemy)
          (exhaust a <> ReleaseChaosTokensCost 1 #any)
    ]

instance RunMessage Bulwark2 where
  runMessage msg a@(Bulwark2 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 windows _ -> do
      cancelAttack attrs (getAttackDetails windows)
      assets <- select $ assetControlledBy iid <> not_ (be attrs) <> AssetCanReady
      chooseTargetM iid assets readyThis
      pure a
    _ -> Bulwark2 <$> liftRunMessage msg attrs

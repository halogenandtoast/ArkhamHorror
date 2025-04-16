module Arkham.Asset.Assets.ZoeysCrossAdvanced (zoeysCrossAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (engagedEnemy)
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype ZoeysCrossAdvanced = ZoeysCrossAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCrossAdvanced :: AssetCard ZoeysCrossAdvanced
zoeysCrossAdvanced = asset ZoeysCrossAdvanced Cards.zoeysCrossAdvanced

instance HasAbilities ZoeysCrossAdvanced where
  getAbilities (ZoeysCrossAdvanced x) =
    [ controlled x 1 CanDealDamage
        $ triggered (EnemyEngaged #after You AnyEnemy) (exhaust x <> ResourceCost 1)
    , controlled x 2 (exists $ EnemyAt YourLocation <> CanEngageEnemy (x.ability 2))
        $ FastAbility (ReturnChaosTokensToPoolCost 2 #bless)
    ]

instance RunMessage ZoeysCrossAdvanced where
  runMessage msg a@(ZoeysCrossAdvanced attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (engagedEnemy -> eid) _ -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectOneToHandle iid (attrs.ability 2)
        $ enemyAtLocationWith iid
        <> CanEngageEnemy (attrs.ability 2)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (EnemyTarget eid) -> do
      push $ EngageEnemy iid eid Nothing False
      pure a
    _ -> ZoeysCrossAdvanced <$> liftRunMessage msg attrs

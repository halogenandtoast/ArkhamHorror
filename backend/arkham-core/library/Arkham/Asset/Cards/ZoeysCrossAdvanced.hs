module Arkham.Asset.Cards.ZoeysCrossAdvanced (ZoeysCrossAdvanced (..), zoeysCrossAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ZoeysCrossAdvanced = ZoeysCrossAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCrossAdvanced :: AssetCard ZoeysCrossAdvanced
zoeysCrossAdvanced = asset ZoeysCrossAdvanced Cards.zoeysCrossAdvanced

instance HasAbilities ZoeysCrossAdvanced where
  getAbilities (ZoeysCrossAdvanced x) =
    [ restrictedAbility x 1 (ControlsThis <> CanDealDamage)
        $ ReactionAbility (EnemyEngaged #after You AnyEnemy)
        $ Costs [exhaust x, ResourceCost 1]
    , controlledAbility x 2 (exists $ EnemyAt YourLocation <> CanEngageEnemy (x.ability 2))
        $ FastAbility (ReturnChaosTokensToPoolCost 2 #bless)
    ]

instance RunMessage ZoeysCrossAdvanced where
  runMessage msg a@(ZoeysCrossAdvanced attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.EnemyEngaged _ eid)] _ -> do
      push $ EnemyDamage eid $ nonAttack attrs 1
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

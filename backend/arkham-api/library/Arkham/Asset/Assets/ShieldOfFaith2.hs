module Arkham.Asset.Assets.ShieldOfFaith2 (shieldOfFaith2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Card
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher

newtype ShieldOfFaith2 = ShieldOfFaith2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shieldOfFaith2 :: AssetCard ShieldOfFaith2
shieldOfFaith2 = assetWith ShieldOfFaith2 Cards.shieldOfFaith2 $ setMeta False

instance HasAbilities ShieldOfFaith2 where
  getAbilities (ShieldOfFaith2 attrs) =
    let active = toResult @Bool attrs.meta
     in restricted
          attrs
          1
          ControlsThis
          ( ReactionAbility
              ( EnemyAttacks
                  #when
                  (affectsOthers $ colocatedWithMatch You)
                  (CancelableEnemyAttack AnyEnemyAttack)
                  AnyEnemy
              )
              (exhaust attrs <> ReleaseChaosTokensCost 1 #any)
          )
          : [restrictedAbility attrs 2 (thisExists attrs AssetWithoutSealedTokens) Anytime | active]

instance RunMessage ShieldOfFaith2 where
  runMessage msg a@(ShieldOfFaith2 attrs) = runQueueT $ case msg of
    ResolvedCard _ card | toCardId card == toCardId attrs -> do
      ShieldOfFaith2 <$> lift (runMessage msg $ setMeta True attrs)
    UseCardAbility _iid (isSource attrs -> True) 1 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 1) details
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure a
    _ -> ShieldOfFaith2 <$> liftRunMessage msg attrs

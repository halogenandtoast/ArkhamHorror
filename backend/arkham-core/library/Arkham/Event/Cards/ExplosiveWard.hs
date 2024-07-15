module Arkham.Event.Cards.ExplosiveWard (explosiveWard, explosiveWardEffect, ExplosiveWard (..)) where

import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype ExplosiveWard = ExplosiveWard EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

explosiveWard :: EventCard ExplosiveWard
explosiveWard = event ExplosiveWard Cards.explosiveWard

instance RunMessage ExplosiveWard where
  runMessage msg e@(ExplosiveWard attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let n = totalResourcePayment attrs.payment
      enemies <- select $ enemyEngagedWith iid <> NonEliteEnemy
      chooseOrRunOne
        iid
        [targetLabel enemy [EnemyDamage enemy $ nonAttack (attrs.ability 1) n] | enemy <- enemies]
      pure e
    _ -> ExplosiveWard <$> liftRunMessage msg attrs

newtype ExplosiveWardEffect = ExplosiveWardEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

explosiveWardEffect :: EffectArgs -> ExplosiveWardEffect
explosiveWardEffect = cardEffect ExplosiveWardEffect Cards.explosiveWard

-- effect is triggered by cdBeforeEffect
instance RunMessage ExplosiveWardEffect where
  runMessage msg e@(ExplosiveWardEffect attrs) = runQueueT case msg of
    CreatedEffect eid _ (InvestigatorSource iid) _target | eid == toId attrs -> do
      assets <- select $ assetControlledBy iid <> AssetInSlot #arcane <> DiscardableAsset
      when (notNull assets) do
        chooseSome
          iid
          "Do not discard any assets"
          [targetLabel a [Discard (Just iid) (toSource attrs) (toTarget a)] | a <- assets]
      disableReturn e
    _ -> ExplosiveWardEffect <$> liftRunMessage msg attrs

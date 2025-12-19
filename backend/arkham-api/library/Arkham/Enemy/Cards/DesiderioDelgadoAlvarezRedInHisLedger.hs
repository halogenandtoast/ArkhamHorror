module Arkham.Enemy.Cards.DesiderioDelgadoAlvarezRedInHisLedger (desiderioDelgadoAlvarezRedInHisLedger) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Trait (Trait (Coterie))

newtype DesiderioDelgadoAlvarezRedInHisLedger = DesiderioDelgadoAlvarezRedInHisLedger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desiderioDelgadoAlvarezRedInHisLedger :: EnemyCard DesiderioDelgadoAlvarezRedInHisLedger
desiderioDelgadoAlvarezRedInHisLedger =
  enemy
    DesiderioDelgadoAlvarezRedInHisLedger
    Cards.desiderioDelgadoAlvarezRedInHisLedger
    (4, Static 5, 3)
    (2, 1)

instance HasAbilities DesiderioDelgadoAlvarezRedInHisLedger where
  getAbilities (DesiderioDelgadoAlvarezRedInHisLedger a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyWouldTakeDamage #when AnySource (EnemyAt (locationWithEnemy a) <> EnemyWithTrait Coterie)

instance RunMessage DesiderioDelgadoAlvarezRedInHisLedger where
  runMessage msg e@(DesiderioDelgadoAlvarezRedInHisLedger attrs) = runQueueT $ case msg of
    InvestigatorDrawEnemy _ eid | eid == attrs.id -> do
      keysFor attrs >>= traverse_ (`createScarletKeyAt_` AttachedToEnemy attrs.id)
      DesiderioDelgadoAlvarezRedInHisLedger <$> liftRunMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 (damagedEnemy -> eid) _ -> do
      damageModifier (attrs.ability 1) eid (DamageTaken (-1))
      nonAttackEnemyDamage Nothing (attrs.ability 1) 1 attrs
      pure e
    _ -> DesiderioDelgadoAlvarezRedInHisLedger <$> liftRunMessage msg attrs

module Arkham.Enemy.Cards.DesiderioDelgadoAlvarezRedInHisLedger (desiderioDelgadoAlvarezRedInHisLedger) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier
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
      $ EnemyWouldTakeDamage #when (EnemyAt (locationWithEnemy a) <> EnemyWithTrait Coterie)

instance RunMessage DesiderioDelgadoAlvarezRedInHisLedger where
  runMessage msg e@(DesiderioDelgadoAlvarezRedInHisLedger attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (damagedEnemy -> eid) _ -> do
      damageModifier (attrs.ability 1) eid (DamageTaken (-1))
      nonAttackEnemyDamage Nothing (attrs.ability 1) 1 attrs
      pure e
    _ -> DesiderioDelgadoAlvarezRedInHisLedger <$> liftRunMessage msg attrs

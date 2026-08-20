module Arkham.Enemy.Cards.TheScarletKeys.RedCoterie.DesiderioDelgadoAlvarezRedInHisLedger (desiderioDelgadoAlvarezRedInHisLedger) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.CardDefs.TheScarletKeys.RedCoterie qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window (damagedEnemy, damagedEnemyAmount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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

instance HasAbilities DesiderioDelgadoAlvarezRedInHisLedger where
  getAbilities (DesiderioDelgadoAlvarezRedInHisLedger a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyWouldTakeDamage
        #when
        AnySource
        (EnemyAt (locationWithEnemy a) <> EnemyWithTrait Coterie <> not_ (be a))

instance RunMessage DesiderioDelgadoAlvarezRedInHisLedger where
  runMessage msg e@(DesiderioDelgadoAlvarezRedInHisLedger attrs) = runQueueT $ case msg of
    InvestigatorDrawEnemy _ eid | eid == attrs.id -> do
      keysFor attrs >>= traverse_ (`createScarletKeyAt_` AttachedToEnemy attrs.id)
      DesiderioDelgadoAlvarezRedInHisLedger <$> liftRunMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (damagedEnemy &&& damagedEnemyAmount -> (eid, n)) _ -> do
      reduceDamageTaken (attrs.ability 1) eid n
      nonAttackEnemyDamage Nothing (attrs.ability 1) n attrs
      skeys <- select $ scarletKeyWithEnemy attrs.id
      chooseOneAtATimeM iid $ targets skeys shift
      pure e
    _ -> DesiderioDelgadoAlvarezRedInHisLedger <$> liftRunMessage msg attrs

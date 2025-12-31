module Arkham.Agenda.Cards.ChaosAtTheCarnevale (chaosAtTheCarnevale) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Window (getEnemy)
import Arkham.Matcher
import Arkham.Strategy

newtype ChaosAtTheCarnevale = ChaosAtTheCarnevale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosAtTheCarnevale :: AgendaCard ChaosAtTheCarnevale
chaosAtTheCarnevale = agenda (3, A) ChaosAtTheCarnevale Cards.chaosAtTheCarnevale (Static 3)

instance HasAbilities ChaosAtTheCarnevale where
  getAbilities (ChaosAtTheCarnevale x) =
    [ mkAbility x 1 $ forced $ EnemySpawns #after Anywhere $ enemyIs Enemies.writhingAppendage
    | onSide A x
    ]

instance RunMessage ChaosAtTheCarnevale where
  runMessage msg a@(ChaosAtTheCarnevale attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemy -> eid) _ -> do
      placeDoom (attrs.ability 1) eid 2
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenJustM (selectOne $ enemyIs Enemies.cnidathqua) \cnidathqua ->
        eachInvestigator \iid -> do
          initiateEnemyAttackEdit cnidathqua attrs iid (damageStrategyL .~ DamageFirst Assets.innocentReveler)
      revertAgenda attrs
      pure a
    _ -> ChaosAtTheCarnevale <$> liftRunMessage msg attrs

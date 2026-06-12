module Arkham.Agenda.Cards.HyperboreanBlood (hyperboreanBlood) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy
import Arkham.Scenarios.BadBlood.Helpers

newtype HyperboreanBlood = HyperboreanBlood AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperboreanBlood :: AgendaCard HyperboreanBlood
hyperboreanBlood = agenda (1, A) HyperboreanBlood Cards.hyperboreanBlood (Static 8)

instance HasAbilities HyperboreanBlood where
  getAbilities (HyperboreanBlood a) =
    [ restricted a 1 (exists $ enemyIs Enemies.elspethBaudin <> ReadyEnemy <> EnemyAt memoryLocation)
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance RunMessage HyperboreanBlood where
  runMessage msg a@(HyperboreanBlood attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      pure a
    RequestedChaosTokens source _ tokens | isSource attrs source -> do
      selectOne (enemyIs Enemies.elspethBaudin) >>= traverse_ \elspeth ->
        withLocationOf elspeth \lid ->
          for_ tokens \token ->
            pushAll [SealChaosToken token, SealedChaosToken token Nothing (toTarget lid)]
      push $ ResetChaosTokens source
      checkMemoryTokens
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioI18n $ scope "agendas.outForBlood" $ flavor $ h "title" >> p "body"
      selectOne (enemyIs Enemies.elspethBaudin) >>= traverse_ \elspeth ->
        eachInvestigator (initiateEnemyAttack elspeth attrs)
      revertAgenda attrs
      pure a
    _ -> HyperboreanBlood <$> liftRunMessage msg attrs

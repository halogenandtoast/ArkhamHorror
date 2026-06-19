module Arkham.Agenda.Cards.HyperboreanBlood (hyperboreanBlood) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Scenarios.BadBlood.Helpers

newtype HyperboreanBlood = HyperboreanBlood AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperboreanBlood :: AgendaCard HyperboreanBlood
hyperboreanBlood = agenda (1, A) HyperboreanBlood Cards.hyperboreanBlood (Static 8)

instance HasAbilities HyperboreanBlood where
  getAbilities (HyperboreanBlood a) =
    [ restricted a 1 (exists $ enemyIs Enemies.elspethBaudin <> #ready <> at_ memoryLocation)
        $ forced
        $ PhaseEnds #when #enemy
    , restricted a 2 (exists $ not_ memoryLocation <> LocationWithPlacedChaosToken AnyChaosToken)
        $ forced AnyWindow
    ]

instance RunMessage HyperboreanBlood where
  runMessage msg a@(HyperboreanBlood attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      checkMemoryTokens
      pure a
    RequestedChaosTokens source@(isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      withMatch (enemyIs Enemies.elspethBaudin) \elspeth ->
        withLocationOf elspeth $ for_ tokens . placeChaosToken
      resetChaosTokens source
      continue_ iid
      checkMemoryTokens
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioI18n $ scope "agendas.outForBlood" $ flavor $ h "title" >> p "body"
      withMatch (enemyIs Enemies.elspethBaudin) \elspeth ->
        eachInvestigator \iid -> initiateEnemyAttackEdit elspeth attrs iid despiteExhausted
      revertAgenda attrs
      pure a
    _ -> HyperboreanBlood <$> liftRunMessage msg attrs

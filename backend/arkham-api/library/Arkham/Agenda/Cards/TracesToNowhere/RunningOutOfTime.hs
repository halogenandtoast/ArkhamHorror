module Arkham.Agenda.Cards.TracesToNowhere.RunningOutOfTime (runningOutOfTime) where

import Arkham.Agenda.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype RunningOutOfTime = RunningOutOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runningOutOfTime :: AgendaCard RunningOutOfTime
runningOutOfTime = agenda (3, A) RunningOutOfTime Cards.runningOutOfTime (Static 8)

instance HasAbilities RunningOutOfTime where
  getAbilities (RunningOutOfTime a) = [abductorDoomWatch a 1, abductInsteadOfDefeat a 2]

instance RunMessage RunningOutOfTime where
  runMessage msg a@(RunningOutOfTime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      abductorDoomWatchRequest (attrs.ability 1)
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      abductorDoomWatchResolve (attrs.ability 1) iid tokens
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      abductInsteadOfDefeatEffect ws
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (kill attrs)
      pure a
    _ -> RunningOutOfTime <$> liftRunMessage msg attrs

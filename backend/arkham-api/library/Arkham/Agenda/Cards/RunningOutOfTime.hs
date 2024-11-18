module Arkham.Agenda.Cards.RunningOutOfTime ( RunningOutOfTime(..) , runningOutOfTime) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype RunningOutOfTime = RunningOutOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runningOutOfTime :: AgendaCard RunningOutOfTime
runningOutOfTime = agenda (3, A) RunningOutOfTime Cards.runningOutOfTime (Static 7)

instance RunMessage RunningOutOfTime where
  runMessage msg a@(RunningOutOfTime attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> RunningOutOfTime <$> liftRunMessage msg attrs

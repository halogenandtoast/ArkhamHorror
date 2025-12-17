module Arkham.Agenda.Cards.RunningRed (runningRed) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype RunningRed = RunningRed AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runningRed :: AgendaCard RunningRed
runningRed = agenda (3, A) RunningRed Cards.runningRed (Static 7)

instance RunMessage RunningRed where
  runMessage msg a@(RunningRed attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (investigatorDefeated attrs)
      pure a
    _ -> RunningRed <$> liftRunMessage msg attrs

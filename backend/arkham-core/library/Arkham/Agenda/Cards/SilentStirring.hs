module Arkham.Agenda.Cards.SilentStirring (SilentStirring (..), silentStirring) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype SilentStirring = SilentStirring AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silentStirring :: AgendaCard SilentStirring
silentStirring = agenda (1, A) SilentStirring Cards.silentStirring (Static 5)

instance RunMessage SilentStirring where
  runMessage msg a@(SilentStirring attrs) = runQueueT do
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        shuffleEncounterDiscardBackIn
        eachInvestigator (raiseAlarmLevel attrs)
        advanceAgendaDeck attrs
        pure a
      _ -> SilentStirring <$> lift (runMessage msg attrs)

module Arkham.Agenda.Cards.SilentStiring (SilentStiring (..), silentStiring) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype SilentStiring = SilentStiring AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silentStiring :: AgendaCard SilentStiring
silentStiring = agenda (1, A) SilentStiring Cards.silentStiring (Static 5)

instance RunMessage SilentStiring where
  runMessage msg a@(SilentStiring attrs) = runQueueT do
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        shuffleEncounterDiscardBackIn
        eachInvestigator (raiseAlarmLevel attrs)
        advanceAgendaDeck attrs
        pure a
      _ -> SilentStiring <$> lift (runMessage msg attrs)

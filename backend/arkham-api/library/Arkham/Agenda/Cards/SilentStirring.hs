module Arkham.Agenda.Cards.SilentStirring (silentStirring) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype SilentStirring = SilentStirring AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silentStirring :: AgendaCard SilentStirring
silentStirring = agenda (1, A) SilentStirring Cards.silentStirring (Static 5)

instance RunMessage SilentStirring where
  runMessage msg a@(SilentStirring attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      raiseAlarmLevel attrs =<< allInvestigators
      advanceAgendaDeck attrs
      pure a
    _ -> SilentStirring <$> liftRunMessage msg attrs

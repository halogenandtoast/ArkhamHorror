module Arkham.Agenda.Cards.TheRitualAdvances (
  TheRitualAdvances (..),
  theRitualAdvances,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheRitualAdvances = TheRitualAdvances AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualAdvances :: AgendaCard TheRitualAdvances
theRitualAdvances = agenda (3, A) TheRitualAdvances Cards.theRitualAdvances (Static 6)

instance RunMessage TheRitualAdvances where
  runMessage msg a@(TheRitualAdvances attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheRitualAdvances <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.PlotsAndPanic (plotsAndPanic) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype PlotsAndPanic = PlotsAndPanic AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plotsAndPanic :: AgendaCard PlotsAndPanic
plotsAndPanic = agenda (4, A) PlotsAndPanic Cards.plotsAndPanic (Static 7)

instance RunMessage PlotsAndPanic where
  runMessage msg a@(PlotsAndPanic attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> PlotsAndPanic <$> liftRunMessage msg attrs

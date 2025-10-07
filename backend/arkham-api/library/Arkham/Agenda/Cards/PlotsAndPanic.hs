module Arkham.Agenda.Cards.PlotsAndPanic (plotsAndPanic) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Act

newtype PlotsAndPanic = PlotsAndPanic AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plotsAndPanic :: AgendaCard PlotsAndPanic
plotsAndPanic = agenda (4, A) PlotsAndPanic Cards.plotsAndPanic (Static 7)

instance RunMessage PlotsAndPanic where
  runMessage msg a@(PlotsAndPanic attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- getCurrentActStep
      push $ if n == 4 then R3 else R4
      pure a
    _ -> PlotsAndPanic <$> liftRunMessage msg attrs

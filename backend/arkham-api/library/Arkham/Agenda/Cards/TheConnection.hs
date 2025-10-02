module Arkham.Agenda.Cards.TheConnection (theConnection) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheConnection = TheConnection AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theConnection :: AgendaCard TheConnection
theConnection = agenda (3, A) TheConnection Cards.theConnection (Static 9)

instance RunMessage TheConnection where
  runMessage msg a@(TheConnection attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheConnection <$> liftRunMessage msg attrs

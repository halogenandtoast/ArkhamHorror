module Arkham.Agenda.Cards.TheWorldUnbidden (theWorldUnbidden) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheWorldUnbidden = TheWorldUnbidden AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWorldUnbidden :: AgendaCard TheWorldUnbidden
theWorldUnbidden = agenda (2, A) TheWorldUnbidden Cards.theWorldUnbidden (Static 10)

instance RunMessage TheWorldUnbidden where
  runMessage msg a@(TheWorldUnbidden attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheWorldUnbidden <$> liftRunMessage msg attrs

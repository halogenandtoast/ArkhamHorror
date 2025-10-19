module Arkham.Agenda.Cards.TheChase (theChase) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheChase = TheChase AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChase :: AgendaCard TheChase
theChase = agenda (1, A) TheChase Cards.theChase (Static 3)

instance RunMessage TheChase where
  runMessage msg a@(TheChase attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheChase <$> liftRunMessage msg attrs

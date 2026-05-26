module Arkham.Agenda.Cards.TheOnslaught (theOnslaught) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheOnslaught = TheOnslaught AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOnslaught :: AgendaCard TheOnslaught
theOnslaught = agenda (1, A) TheOnslaught Cards.theOnslaught (Static 10)

instance RunMessage TheOnslaught where
  runMessage msg a@(TheOnslaught attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheOnslaught <$> liftRunMessage msg attrs

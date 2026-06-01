module Arkham.Agenda.Cards.TheSpiral (theSpiral) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheSpiral = TheSpiral AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSpiral :: AgendaCard TheSpiral
theSpiral = agenda (3, A) TheSpiral Cards.theSpiral (Static 4)

instance RunMessage TheSpiral where
  runMessage msg (TheSpiral attrs) =
    runQueueT $ TheSpiral <$> liftRunMessage msg attrs

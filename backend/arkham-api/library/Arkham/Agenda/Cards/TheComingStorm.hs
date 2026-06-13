module Arkham.Agenda.Cards.TheComingStorm (theComingStorm) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheComingStorm = TheComingStorm AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theComingStorm :: AgendaCard TheComingStorm
theComingStorm = agenda (1, A) TheComingStorm Cards.theComingStorm (Static 14)

-- TODO: abilities

instance RunMessage TheComingStorm where
  runMessage msg (TheComingStorm attrs) = runQueueT $ TheComingStorm <$> liftRunMessage msg attrs

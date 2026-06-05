module Arkham.Agenda.Cards.TheAnomalySwells (theAnomalySwells) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheAnomalySwells = TheAnomalySwells AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAnomalySwells :: AgendaCard TheAnomalySwells
theAnomalySwells = agenda (2, A) TheAnomalySwells Cards.theAnomalySwells (Static 6)

instance RunMessage TheAnomalySwells where
  runMessage msg (TheAnomalySwells attrs) = TheAnomalySwells <$> runMessage msg attrs

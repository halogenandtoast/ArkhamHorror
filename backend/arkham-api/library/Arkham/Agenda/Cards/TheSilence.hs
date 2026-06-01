module Arkham.Agenda.Cards.TheSilence (theSilence) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheSilence = TheSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilence :: AgendaCard TheSilence
theSilence = agenda (1, A) TheSilence Cards.theSilence (Static 6)

instance RunMessage TheSilence where
  runMessage msg (TheSilence attrs) =
    runQueueT $ TheSilence <$> liftRunMessage msg attrs

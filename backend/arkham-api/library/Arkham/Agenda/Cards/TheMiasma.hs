module Arkham.Agenda.Cards.TheMiasma (theMiasma) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheMiasma = TheMiasma AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMiasma :: AgendaCard TheMiasma
theMiasma = agenda (2, A) TheMiasma Cards.theMiasma (Static 5)

instance RunMessage TheMiasma where
  runMessage msg (TheMiasma attrs) =
    runQueueT $ TheMiasma <$> liftRunMessage msg attrs

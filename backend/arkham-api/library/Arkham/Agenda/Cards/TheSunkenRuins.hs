module Arkham.Agenda.Cards.TheSunkenRuins (theSunkenRuins) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheSunkenRuins = TheSunkenRuins AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSunkenRuins :: AgendaCard TheSunkenRuins
theSunkenRuins = agenda (1, A) TheSunkenRuins Cards.theSunkenRuins (Static 7)

-- TODO: abilities

instance RunMessage TheSunkenRuins where
  runMessage msg (TheSunkenRuins attrs) = runQueueT $ TheSunkenRuins <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.CthulhuAwakened (cthulhuAwakened) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype CthulhuAwakened = CthulhuAwakened AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuAwakened :: AgendaCard CthulhuAwakened
cthulhuAwakened = agenda (2, A) CthulhuAwakened Cards.cthulhuAwakened (Static 100)

-- TODO: abilities

instance RunMessage CthulhuAwakened where
  runMessage msg (CthulhuAwakened attrs) = runQueueT $ CthulhuAwakened <$> liftRunMessage msg attrs

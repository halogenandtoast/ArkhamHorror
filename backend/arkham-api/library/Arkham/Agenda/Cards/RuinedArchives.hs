module Arkham.Agenda.Cards.RuinedArchives (ruinedArchives) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype RuinedArchives = RuinedArchives AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinedArchives :: AgendaCard RuinedArchives
ruinedArchives = agenda (1, A) RuinedArchives Cards.ruinedArchives (Static 6)

-- TODO: abilities

instance RunMessage RuinedArchives where
  runMessage msg (RuinedArchives attrs) = runQueueT $ RuinedArchives <$> liftRunMessage msg attrs

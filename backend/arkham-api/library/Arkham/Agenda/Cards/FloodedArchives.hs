module Arkham.Agenda.Cards.FloodedArchives (floodedArchives) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype FloodedArchives = FloodedArchives AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedArchives :: AgendaCard FloodedArchives
floodedArchives = agenda (1, A) FloodedArchives Cards.floodedArchives (Static 6)

-- TODO: abilities

instance RunMessage FloodedArchives where
  runMessage msg (FloodedArchives attrs) = runQueueT $ FloodedArchives <$> liftRunMessage msg attrs

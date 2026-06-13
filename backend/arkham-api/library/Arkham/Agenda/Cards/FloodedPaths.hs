module Arkham.Agenda.Cards.FloodedPaths (floodedPaths) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype FloodedPaths = FloodedPaths AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedPaths :: AgendaCard FloodedPaths
floodedPaths = agenda (1, A) FloodedPaths Cards.floodedPaths (Static 13)

-- TODO: abilities

instance RunMessage FloodedPaths where
  runMessage msg (FloodedPaths attrs) = runQueueT $ FloodedPaths <$> liftRunMessage msg attrs

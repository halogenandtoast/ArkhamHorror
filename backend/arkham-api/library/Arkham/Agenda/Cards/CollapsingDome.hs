module Arkham.Agenda.Cards.CollapsingDome (collapsingDome) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype CollapsingDome = CollapsingDome AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collapsingDome :: AgendaCard CollapsingDome
collapsingDome = agenda (2, A) CollapsingDome Cards.collapsingDome (Static 7)

-- TODO: abilities

instance RunMessage CollapsingDome where
  runMessage msg (CollapsingDome attrs) = runQueueT $ CollapsingDome <$> liftRunMessage msg attrs

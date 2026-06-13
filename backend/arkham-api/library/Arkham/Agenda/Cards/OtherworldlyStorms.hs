module Arkham.Agenda.Cards.OtherworldlyStorms (otherworldlyStorms) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype OtherworldlyStorms = OtherworldlyStorms AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyStorms :: AgendaCard OtherworldlyStorms
otherworldlyStorms = agenda (1, A) OtherworldlyStorms Cards.otherworldlyStorms (Static 3)

-- TODO: abilities

instance RunMessage OtherworldlyStorms where
  runMessage msg (OtherworldlyStorms attrs) = runQueueT $ OtherworldlyStorms <$> liftRunMessage msg attrs

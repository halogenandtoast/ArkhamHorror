module Arkham.Agenda.Cards.UnstableFoundations (unstableFoundations) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype UnstableFoundations = UnstableFoundations AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unstableFoundations :: AgendaCard UnstableFoundations
unstableFoundations = agenda (2, A) UnstableFoundations Cards.unstableFoundations (Static 8)

-- TODO: abilities

instance RunMessage UnstableFoundations where
  runMessage msg (UnstableFoundations attrs) = runQueueT $ UnstableFoundations <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.EncroachingStorms (encroachingStorms) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EncroachingStorms = EncroachingStorms AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encroachingStorms :: AgendaCard EncroachingStorms
encroachingStorms = agenda (1, A) EncroachingStorms Cards.encroachingStorms (Static 3)

-- TODO: abilities

instance RunMessage EncroachingStorms where
  runMessage msg (EncroachingStorms attrs) = runQueueT $ EncroachingStorms <$> liftRunMessage msg attrs

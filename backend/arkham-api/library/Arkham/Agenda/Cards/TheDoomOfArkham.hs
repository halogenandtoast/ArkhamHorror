module Arkham.Agenda.Cards.TheDoomOfArkham (theDoomOfArkham) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheDoomOfArkham = TheDoomOfArkham AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfArkham :: AgendaCard TheDoomOfArkham
theDoomOfArkham = agenda (1, A) TheDoomOfArkham Cards.theDoomOfArkham (Static 3)

-- TODO: abilities

instance RunMessage TheDoomOfArkham where
  runMessage msg (TheDoomOfArkham attrs) = runQueueT $ TheDoomOfArkham <$> liftRunMessage msg attrs

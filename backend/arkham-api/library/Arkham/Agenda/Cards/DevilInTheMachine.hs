module Arkham.Agenda.Cards.DevilInTheMachine (devilInTheMachine) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DevilInTheMachine = DevilInTheMachine AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilInTheMachine :: AgendaCard DevilInTheMachine
devilInTheMachine = agenda (2, A) DevilInTheMachine Cards.devilInTheMachine (Static 5)

-- TODO: abilities

instance RunMessage DevilInTheMachine where
  runMessage msg (DevilInTheMachine attrs) = runQueueT $ DevilInTheMachine <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.RestlessDead (restlessDead) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype RestlessDead = RestlessDead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessDead :: AgendaCard RestlessDead
restlessDead = agenda (2, A) RestlessDead Cards.restlessDead (Static 3)

instance RunMessage RestlessDead where
  runMessage msg a@(RestlessDead attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> RestlessDead <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.TheChaseIsOnV1
  ( TheChaseIsOnV1(..)
  , theChaseIsOnV1
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheChaseIsOnV1 = TheChaseIsOnV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChaseIsOnV1 :: AgendaCard TheChaseIsOnV1
theChaseIsOnV1 = agenda (1, A) TheChaseIsOnV1 Cards.theChaseIsOnV1 (Static 12)

instance RunMessage TheChaseIsOnV1 where
  runMessage msg a@(TheChaseIsOnV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheChaseIsOnV1 <$> liftRunMessage msg attrs

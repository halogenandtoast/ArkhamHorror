module Arkham.Agenda.Cards.TheChaseIsOnV2
  ( TheChaseIsOnV2(..)
  , theChaseIsOnV2
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheChaseIsOnV2 = TheChaseIsOnV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChaseIsOnV2 :: AgendaCard TheChaseIsOnV2
theChaseIsOnV2 = agenda (1, A) TheChaseIsOnV2 Cards.theChaseIsOnV2 (Static 12)

instance RunMessage TheChaseIsOnV2 where
  runMessage msg a@(TheChaseIsOnV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheChaseIsOnV2 <$> liftRunMessage msg attrs

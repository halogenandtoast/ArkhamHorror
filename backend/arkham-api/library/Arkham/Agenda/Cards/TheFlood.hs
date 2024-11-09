module Arkham.Agenda.Cards.TheFlood
  ( TheFlood(..)
  , theFlood
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheFlood = TheFlood AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFlood :: AgendaCard TheFlood
theFlood = agenda (3, A) TheFlood Cards.theFlood (Static 10)

instance RunMessage TheFlood where
  runMessage msg a@(TheFlood attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheFlood <$> liftRunMessage msg attrs

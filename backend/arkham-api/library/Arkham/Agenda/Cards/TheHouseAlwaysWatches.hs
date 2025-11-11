module Arkham.Agenda.Cards.TheHouseAlwaysWatches (theHouseAlwaysWatches) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheHouseAlwaysWatches = TheHouseAlwaysWatches AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseAlwaysWatches :: AgendaCard TheHouseAlwaysWatches
theHouseAlwaysWatches = agenda (1, A) TheHouseAlwaysWatches Cards.theHouseAlwaysWatches (Static 12)

instance RunMessage TheHouseAlwaysWatches where
  runMessage msg a@(TheHouseAlwaysWatches attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseAlwaysWatches <$> liftRunMessage msg attrs

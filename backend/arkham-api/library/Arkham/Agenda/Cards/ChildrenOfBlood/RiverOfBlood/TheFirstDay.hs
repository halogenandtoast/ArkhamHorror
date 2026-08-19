module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheFirstDay (theFirstDay) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheFirstDay = TheFirstDay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstDay :: AgendaCard TheFirstDay
theFirstDay = agenda (1, A) TheFirstDay Cards.theFirstDay (Static 4)

instance RunMessage TheFirstDay where
  runMessage msg a@(TheFirstDay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheFirstDay <$> liftRunMessage msg attrs

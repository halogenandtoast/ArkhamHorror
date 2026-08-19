module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheSecondDay (theSecondDay) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheSecondDay = TheSecondDay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondDay :: AgendaCard TheSecondDay
theSecondDay = agenda (3, A) TheSecondDay Cards.theSecondDay (Static 4)

instance RunMessage TheSecondDay where
  runMessage msg a@(TheSecondDay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheSecondDay <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.TimeCollapsing (timeCollapsing) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TimeCollapsing = TimeCollapsing AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeCollapsing :: AgendaCard TimeCollapsing
timeCollapsing = agenda (3, A) TimeCollapsing Cards.timeCollapsing (Static 6)

instance RunMessage TimeCollapsing where
  runMessage msg a@(TimeCollapsing attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> TimeCollapsing <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.EmptyStreets (emptyStreets) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EmptyStreets = EmptyStreets AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emptyStreets :: AgendaCard EmptyStreets
emptyStreets = agenda (2, A) EmptyStreets Cards.emptyStreets (Static 7)

instance RunMessage EmptyStreets where
  runMessage msg a@(EmptyStreets attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> EmptyStreets <$> liftRunMessage msg attrs

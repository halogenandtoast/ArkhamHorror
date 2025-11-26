module Arkham.Agenda.Cards.LostAndForgotten (lostAndForgotten) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype LostAndForgotten = LostAndForgotten AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostAndForgotten :: AgendaCard LostAndForgotten
lostAndForgotten = agenda (1, A) LostAndForgotten Cards.lostAndForgotten (Static 7)

instance RunMessage LostAndForgotten where
  runMessage msg a@(LostAndForgotten attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> LostAndForgotten <$> liftRunMessage msg attrs

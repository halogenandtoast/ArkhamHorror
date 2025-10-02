module Arkham.Agenda.Cards.FiguresInTheFog (figuresInTheFog) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype FiguresInTheFog = FiguresInTheFog AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

figuresInTheFog :: AgendaCard FiguresInTheFog
figuresInTheFog = agenda (2, A) FiguresInTheFog Cards.figuresInTheFog (Static 4)

instance RunMessage FiguresInTheFog where
  runMessage msg a@(FiguresInTheFog attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> FiguresInTheFog <$> liftRunMessage msg attrs

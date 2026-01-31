module Arkham.Agenda.Cards.DeepeningDark (deepeningDark) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DeepeningDark = DeepeningDark AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepeningDark :: AgendaCard DeepeningDark
deepeningDark = agenda (1, A) DeepeningDark Cards.deepeningDark (Static 12)

instance RunMessage DeepeningDark where
  runMessage msg a@(DeepeningDark attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> DeepeningDark <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.PainfulHistory (painfulHistory) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype PainfulHistory = PainfulHistory AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painfulHistory :: AgendaCard PainfulHistory
painfulHistory = agenda (1, A) PainfulHistory Cards.painfulHistory (Static 3)

instance RunMessage PainfulHistory where
  runMessage msg a@(PainfulHistory attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> PainfulHistory <$> liftRunMessage msg attrs

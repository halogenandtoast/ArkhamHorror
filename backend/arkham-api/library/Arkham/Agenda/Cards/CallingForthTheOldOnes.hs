module Arkham.Agenda.Cards.CallingForthTheOldOnes (callingForthTheOldOnes) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype CallingForthTheOldOnes = CallingForthTheOldOnes AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingForthTheOldOnes :: AgendaCard CallingForthTheOldOnes
callingForthTheOldOnes = agenda (1, A) CallingForthTheOldOnes Cards.callingForthTheOldOnes (Static 12)

instance RunMessage CallingForthTheOldOnes where
  runMessage msg a@(CallingForthTheOldOnes attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> CallingForthTheOldOnes <$> liftRunMessage msg attrs

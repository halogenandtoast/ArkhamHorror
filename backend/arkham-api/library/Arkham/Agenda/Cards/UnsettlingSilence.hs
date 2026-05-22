module Arkham.Agenda.Cards.UnsettlingSilence (unsettlingSilence) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype UnsettlingSilence = UnsettlingSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsettlingSilence :: AgendaCard UnsettlingSilence
unsettlingSilence = agenda (1, A) UnsettlingSilence Cards.unsettlingSilence (Static 8)

instance RunMessage UnsettlingSilence where
  runMessage msg a@(UnsettlingSilence attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> UnsettlingSilence <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.TheTurn (theTurn) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheTurn = TheTurn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTurn :: AgendaCard TheTurn
theTurn = agenda (3, A) TheTurn Cards.theTurn (Static 12)

instance RunMessage TheTurn where
  runMessage msg a@(TheTurn attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheTurn <$> liftRunMessage msg attrs

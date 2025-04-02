module Arkham.Agenda.Cards.CallOfMadness (callOfMadness) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype CallOfMadness = CallOfMadness AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfMadness :: AgendaCard CallOfMadness
callOfMadness = agenda (3, A) CallOfMadness Cards.callOfMadness (Static 6)

instance RunMessage CallOfMadness where
  runMessage msg a@(CallOfMadness attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> CallOfMadness <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.FearTheReaper (fearTheReaper) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype FearTheReaper = FearTheReaper AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearTheReaper :: AgendaCard FearTheReaper
fearTheReaper = agenda (3, A) FearTheReaper Cards.fearTheReaper (Static 4)

instance RunMessage FearTheReaper where
  runMessage msg a@(FearTheReaper attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> FearTheReaper <$> liftRunMessage msg attrs

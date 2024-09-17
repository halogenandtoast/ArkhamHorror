module Arkham.Agenda.Cards.FranticPursuit
  ( FranticPursuit(..)
  , franticPursuit
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype FranticPursuit = FranticPursuit AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

franticPursuit :: AgendaCard FranticPursuit
franticPursuit = agenda (3, A) FranticPursuit Cards.franticPursuit (Static 7)

instance RunMessage FranticPursuit where
  runMessage msg a@(FranticPursuit attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> FranticPursuit <$> liftRunMessage msg attrs

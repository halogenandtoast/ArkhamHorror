module Arkham.Agenda.Cards.HotPursuit
  ( HotPursuit(..)
  , hotPursuit
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype HotPursuit = HotPursuit AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotPursuit :: AgendaCard HotPursuit
hotPursuit = agenda (1, A) HotPursuit Cards.hotPursuit (Static 12)

instance RunMessage HotPursuit where
  runMessage msg a@(HotPursuit attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> HotPursuit <$> liftRunMessage msg attrs

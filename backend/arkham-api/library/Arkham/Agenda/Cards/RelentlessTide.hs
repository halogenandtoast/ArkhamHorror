module Arkham.Agenda.Cards.RelentlessTide
  ( RelentlessTide(..)
  , relentlessTide
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype RelentlessTide = RelentlessTide AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentlessTide :: AgendaCard RelentlessTide
relentlessTide = agenda (2, A) RelentlessTide Cards.relentlessTide (Static 12)

instance RunMessage RelentlessTide where
  runMessage msg a@(RelentlessTide attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> RelentlessTide <$> liftRunMessage msg attrs

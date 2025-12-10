module Arkham.Agenda.Cards.OtherworldlyHorror (otherworldlyHorror) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype OtherworldlyHorror = OtherworldlyHorror AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyHorror :: AgendaCard OtherworldlyHorror
otherworldlyHorror = agenda (1, A) OtherworldlyHorror Cards.otherworldlyHorror (Static 5)

instance RunMessage OtherworldlyHorror where
  runMessage msg a@(OtherworldlyHorror attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> OtherworldlyHorror <$> liftRunMessage msg attrs

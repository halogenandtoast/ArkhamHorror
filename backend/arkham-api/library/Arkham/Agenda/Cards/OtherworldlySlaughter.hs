module Arkham.Agenda.Cards.OtherworldlySlaughter (otherworldlySlaughter) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype OtherworldlySlaughter = OtherworldlySlaughter AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlySlaughter :: AgendaCard OtherworldlySlaughter
otherworldlySlaughter = agenda (3, A) OtherworldlySlaughter Cards.otherworldlySlaughter (Static 6)

instance RunMessage OtherworldlySlaughter where
  runMessage msg a@(OtherworldlySlaughter attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> OtherworldlySlaughter <$> liftRunMessage msg attrs

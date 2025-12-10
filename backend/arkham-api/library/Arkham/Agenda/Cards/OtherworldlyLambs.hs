module Arkham.Agenda.Cards.OtherworldlyLambs (otherworldlyLambs) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype OtherworldlyLambs = OtherworldlyLambs AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyLambs :: AgendaCard OtherworldlyLambs
otherworldlyLambs = agenda (2, A) OtherworldlyLambs Cards.otherworldlyLambs (Static 5)

instance RunMessage OtherworldlyLambs where
  runMessage msg a@(OtherworldlyLambs attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> OtherworldlyLambs <$> liftRunMessage msg attrs

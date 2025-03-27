module Arkham.Agenda.Cards.DoomFromBelow (doomFromBelow) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DoomFromBelow = DoomFromBelow AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doomFromBelow :: AgendaCard DoomFromBelow
doomFromBelow = agenda (1, A) DoomFromBelow Cards.doomFromBelow (Static 10)

instance RunMessage DoomFromBelow where
  runMessage msg a@(DoomFromBelow attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> DoomFromBelow <$> liftRunMessage msg attrs

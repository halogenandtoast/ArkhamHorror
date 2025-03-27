module Arkham.Agenda.Cards.LurkingHorrors (lurkingHorrors) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype LurkingHorrors = LurkingHorrors AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lurkingHorrors :: AgendaCard LurkingHorrors
lurkingHorrors = agenda (1, A) LurkingHorrors Cards.lurkingHorrors (Static 6)

instance RunMessage LurkingHorrors where
  runMessage msg a@(LurkingHorrors attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> LurkingHorrors <$> liftRunMessage msg attrs

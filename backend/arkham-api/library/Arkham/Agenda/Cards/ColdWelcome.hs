module Arkham.Agenda.Cards.ColdWelcome ( ColdWelcome(..) , coldWelcome) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ColdWelcome = ColdWelcome AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldWelcome :: AgendaCard ColdWelcome
coldWelcome = agenda (1, A) ColdWelcome Cards.coldWelcome (Static 4)

instance RunMessage ColdWelcome where
  runMessage msg a@(ColdWelcome attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ColdWelcome <$> liftRunMessage msg attrs

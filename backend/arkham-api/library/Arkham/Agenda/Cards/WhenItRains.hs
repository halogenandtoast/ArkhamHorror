module Arkham.Agenda.Cards.WhenItRains (whenItRains) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype WhenItRains = WhenItRains AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whenItRains :: AgendaCard WhenItRains
whenItRains = agenda (1, A) WhenItRains Cards.whenItRains (Static 2)

instance RunMessage WhenItRains where
  runMessage msg a@(WhenItRains attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> WhenItRains <$> liftRunMessage msg attrs

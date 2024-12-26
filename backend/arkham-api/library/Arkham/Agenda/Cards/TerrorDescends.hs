module Arkham.Agenda.Cards.TerrorDescends (terrorDescends) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TerrorDescends = TerrorDescends AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorDescends :: AgendaCard TerrorDescends
terrorDescends = agenda (2, A) TerrorDescends Cards.terrorDescends (Static 8)

instance RunMessage TerrorDescends where
  runMessage msg a@(TerrorDescends attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TerrorDescends <$> liftRunMessage msg attrs

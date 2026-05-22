module Arkham.Agenda.Cards.DesolationV2 (desolationV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DesolationV2 = DesolationV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desolationV2 :: AgendaCard DesolationV2
desolationV2 = agenda (2, A) DesolationV2 Cards.desolationV2 (Static 7)

instance RunMessage DesolationV2 where
  runMessage msg a@(DesolationV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> DesolationV2 <$> liftRunMessage msg attrs

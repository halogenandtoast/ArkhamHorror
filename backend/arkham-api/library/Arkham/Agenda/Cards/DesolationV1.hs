module Arkham.Agenda.Cards.DesolationV1 (desolationV1) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DesolationV1 = DesolationV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desolationV1 :: AgendaCard DesolationV1
desolationV1 = agenda (2, A) DesolationV1 Cards.desolationV1 (Static 7)

instance RunMessage DesolationV1 where
  runMessage msg a@(DesolationV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> DesolationV1 <$> liftRunMessage msg attrs

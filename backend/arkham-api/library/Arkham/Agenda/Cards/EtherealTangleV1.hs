module Arkham.Agenda.Cards.EtherealTangleV1 (etherealTangleV1) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EtherealTangleV1 = EtherealTangleV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealTangleV1 :: AgendaCard EtherealTangleV1
etherealTangleV1 = agenda (1, A) EtherealTangleV1 Cards.etherealTangleV1 (Static 15)

instance RunMessage EtherealTangleV1 where
  runMessage msg a@(EtherealTangleV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> EtherealTangleV1 <$> liftRunMessage msg attrs

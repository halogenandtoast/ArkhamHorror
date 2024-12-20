module Arkham.Agenda.Cards.EtherealTangleV2 (etherealTangleV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EtherealTangleV2 = EtherealTangleV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealTangleV2 :: AgendaCard EtherealTangleV2
etherealTangleV2 = agenda (2, A) EtherealTangleV2 Cards.etherealTangleV2 (Static 13)

instance RunMessage EtherealTangleV2 where
  runMessage msg a@(EtherealTangleV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> EtherealTangleV2 <$> liftRunMessage msg attrs

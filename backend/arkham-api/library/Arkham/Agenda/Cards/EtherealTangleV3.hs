module Arkham.Agenda.Cards.EtherealTangleV3 (etherealTangleV3) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EtherealTangleV3 = EtherealTangleV3 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealTangleV3 :: AgendaCard EtherealTangleV3
etherealTangleV3 = agenda (3, A) EtherealTangleV3 Cards.etherealTangleV3 (Static 11)

instance RunMessage EtherealTangleV3 where
  runMessage msg a@(EtherealTangleV3 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> EtherealTangleV3 <$> liftRunMessage msg attrs

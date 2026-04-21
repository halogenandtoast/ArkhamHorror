module Arkham.Agenda.Cards.TheHouseStirsV1 (theHouseStirsV1) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheHouseStirsV1 = TheHouseStirsV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseStirsV1 :: AgendaCard TheHouseStirsV1
theHouseStirsV1 = agenda (2, A) TheHouseStirsV1 Cards.theHouseStirsV1 (Static 5)

instance RunMessage TheHouseStirsV1 where
  runMessage msg a@(TheHouseStirsV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseStirsV1 <$> liftRunMessage msg attrs

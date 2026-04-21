module Arkham.Agenda.Cards.TheHouseStirsV2 (theHouseStirsV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheHouseStirsV2 = TheHouseStirsV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseStirsV2 :: AgendaCard TheHouseStirsV2
theHouseStirsV2 = agenda (2, A) TheHouseStirsV2 Cards.theHouseStirsV2 (Static 5)

instance RunMessage TheHouseStirsV2 where
  runMessage msg a@(TheHouseStirsV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseStirsV2 <$> liftRunMessage msg attrs

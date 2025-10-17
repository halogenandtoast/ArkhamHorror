module Arkham.Agenda.Cards.SeeingRed (seeingRed) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype SeeingRed = SeeingRed AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seeingRed :: AgendaCard SeeingRed
seeingRed = agenda (3, A) SeeingRed Cards.seeingRed (Static 11)

instance RunMessage SeeingRed where
  runMessage msg a@(SeeingRed attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> SeeingRed <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.AllBetsDown (allBetsDown) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype AllBetsDown = AllBetsDown AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allBetsDown :: AgendaCard AllBetsDown
allBetsDown = agenda (4, A) AllBetsDown Cards.allBetsDown (Static 12)

instance RunMessage AllBetsDown where
  runMessage msg a@(AllBetsDown attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> AllBetsDown <$> liftRunMessage msg attrs

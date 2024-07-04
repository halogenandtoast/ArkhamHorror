module Arkham.Agenda.Cards.TheyAreUponYou (TheyAreUponYou (..), theyAreUponYou) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype TheyAreUponYou = TheyAreUponYou AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyAreUponYou :: AgendaCard TheyAreUponYou
theyAreUponYou = agenda (3, A) TheyAreUponYou Cards.theyAreUponYou (Static 4)

instance RunMessage TheyAreUponYou where
  runMessage msg a@(TheyAreUponYou attrs) = runQueueT do
    case msg of
      AdvanceAgenda (isSide B attrs -> True) -> do
        eachInvestigator (raiseAlarmLevel attrs)
        eachInvestigator \iid -> do
          alarmLevel <- getAlarmLevel iid
          let horror = (alarmLevel + 1) `div` 2
          assignHorror iid attrs horror

        revertAgenda attrs
        placeDoomOnAgenda 2
        pure a
      _ -> TheyAreUponYou <$> liftRunMessage msg attrs

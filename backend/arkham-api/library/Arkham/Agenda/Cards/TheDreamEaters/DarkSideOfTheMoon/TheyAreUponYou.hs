module Arkham.Agenda.Cards.TheDreamEaters.DarkSideOfTheMoon.TheyAreUponYou (TheyAreUponYou (..), theyAreUponYou) where

import Arkham.Agenda.CardDefs.TheDreamEaters.DarkSideOfTheMoon qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Scenarios.TheDreamEaters.DarkSideOfTheMoon.Helpers

newtype TheyAreUponYou = TheyAreUponYou AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyAreUponYou :: AgendaCard TheyAreUponYou
theyAreUponYou = agenda (3, A) TheyAreUponYou Cards.theyAreUponYou (Static 4)

instance RunMessage TheyAreUponYou where
  runMessage msg a@(TheyAreUponYou attrs) = runQueueT do
    case msg of
      AdvanceAgenda (isSide B attrs -> True) -> do
        raiseAlarmLevel attrs =<< allInvestigators
        eachInvestigator \iid -> do
          alarmLevel <- getAlarmLevel iid
          let horror = (alarmLevel + 1) `div` 2
          assignHorror iid attrs horror

        revertAgenda attrs
        placeDoomOnAgenda 2
        pure a
      _ -> TheyAreUponYou <$> liftRunMessage msg attrs

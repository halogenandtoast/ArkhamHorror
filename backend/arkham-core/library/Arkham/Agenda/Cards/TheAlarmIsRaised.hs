module Arkham.Agenda.Cards.TheAlarmIsRaised (TheAlarmIsRaised (..), theAlarmIsRaised) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.CampaignLogKey (CampaignLogKey (TheInvestigatorsForcedTheirWayIntoTheTemple))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log (whenHasRecord)
import Arkham.Helpers.Query (getLead)
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype TheAlarmIsRaised = TheAlarmIsRaised AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAlarmIsRaised :: AgendaCard TheAlarmIsRaised
theAlarmIsRaised = agenda (2, A) TheAlarmIsRaised Cards.theAlarmIsRaised (Static 5)

instance RunMessage TheAlarmIsRaised where
  runMessage msg a@(TheAlarmIsRaised attrs) = runQueueT do
    case msg of
      AdvanceAgenda (isSide B attrs -> True) -> do
        shuffleEncounterDiscardBackIn
        eachInvestigator (raiseAlarmLevel attrs)
        whenHasRecord TheInvestigatorsForcedTheirWayIntoTheTemple do
          lead <- getLead
          findAndDrawEncounterCard lead Enemies.catsFromSaturn
        advanceAgendaDeck attrs
        pure a
      _ -> TheAlarmIsRaised <$> liftRunMessage msg attrs

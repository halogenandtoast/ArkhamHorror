module Arkham.Agenda.Cards.RunningOutOfTime (RunningOutOfTime (..), runningOutOfTime) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Helpers.Window (entering)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.IceAndDeath.Helpers

newtype RunningOutOfTime = RunningOutOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runningOutOfTime :: AgendaCard RunningOutOfTime
runningOutOfTime = agenda (3, A) RunningOutOfTime Cards.runningOutOfTime (Static 7)

instance HasAbilities RunningOutOfTime where
  getAbilities (RunningOutOfTime a) = [placeSetAsideConnectedAbility a 1]

instance RunMessage RunningOutOfTime where
  runMessage msg a@(RunningOutOfTime attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      allInvestigators >>= traverse_ \iid -> do
        addTekelili iid =<< getTekelili 1
      locations <- select $ LocationWithoutClues

      if null locations
        then do
          crashSite <- selectJust $ locationIs Locations.crashSite
          forTarget crashSite $ push msg
        else do
          lead <- getLead
          chooseOrRunOneM lead do
            targets locations \lid -> forTarget lid $ push msg

      pure a
    ForTarget (LocationTarget lid) (AdvanceAgenda (isSide B attrs -> True)) -> do
      eachInvestigator \iid -> do
        moveTo_ attrs iid lid
        resign iid
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 (entering -> lid) _ -> do
      placeSetAsideConnectedLocations lid
      pure a
    _ -> RunningOutOfTime <$> liftRunMessage msg attrs

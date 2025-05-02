module Arkham.Agenda.Cards.TheFirstNight (theFirstNight) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype TheFirstNight = TheFirstNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstNight :: AgendaCard TheFirstNight
theFirstNight = agenda (1, A) TheFirstNight Cards.theFirstNight (Static 6)

instance HasModifiersFor TheFirstNight where
  getModifiersFor (TheFirstNight a) = do
    moreConvictionThanDoubt <- getMoreConvictionThanDoubt
    modifySelf a [OtherDoomSubtracts | moreConvictionThanDoubt]

instance RunMessage TheFirstNight where
  runMessage msg a@(TheFirstNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      disengageEachEnemyAndMoveToConnectingLocation attrs
      doStep 1 msg
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      moveOrganistAwayFromNearestInvestigator
      pure a
    _ -> TheFirstNight <$> liftRunMessage msg attrs

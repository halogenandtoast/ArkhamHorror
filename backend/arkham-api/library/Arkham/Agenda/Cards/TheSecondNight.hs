module Arkham.Agenda.Cards.TheSecondNight (theSecondNight) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype TheSecondNight = TheSecondNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondNight :: AgendaCard TheSecondNight
theSecondNight = agenda (2, A) TheSecondNight Cards.theSecondNight (Static 5)

instance HasModifiersFor TheSecondNight where
  getModifiersFor (TheSecondNight a) = do
    moreConvictionThanDoubt <- getMoreConvictionThanDoubt
    modifySelf a [OtherDoomSubtracts | moreConvictionThanDoubt]

instance RunMessage TheSecondNight where
  runMessage msg a@(TheSecondNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      disengageEachEnemyAndMoveToConnectingLocation attrs
      doStep 1 msg
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      moveOrganistAwayFromNearestInvestigator
      whenM (not <$> slain Enemies.jordanPerry) do
        createEnemyAtLocationMatching_ Enemies.jordanPerry (LocationWithTitle "Montparnasse")
      pure a
    _ -> TheSecondNight <$> liftRunMessage msg attrs

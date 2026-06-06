module Arkham.Agenda.Cards.TheAnomalySwells (theAnomalySwells) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers (canDevourLocation)
import Arkham.Trait (Trait (Ooze, Oozified))

newtype TheAnomalySwells = TheAnomalySwells AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAnomalySwells :: AgendaCard TheAnomalySwells
theAnomalySwells = agenda (2, A) TheAnomalySwells Cards.theAnomalySwells (Static 6)

instance RunMessage TheAnomalySwells where
  runMessage msg a@(TheAnomalySwells attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      doStep 2 msg
      shuffleSetAsideIntoEncounterDeck $ cardIs Enemies.oozewraith
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    DoStep n msg'@(AdvanceAgenda (isSide B attrs -> True)) | n > 0 -> do
      locations <-
        select
          $ LocationWithTrait Oozified
          <> not_ (locationIs Locations.theCrater)
          <> not_ (LocationWithEnemy (enemyIs Enemies.vulnerableHeart))
      validLocations <- filterM canDevourLocation locations
      leadChooseOneM $ targets validLocations \lid -> do
        selectEach (enemyAt lid <> EnemyWithTrait Ooze) (toDiscard attrs)
        selectEach (enemyAt lid <> not_ (EnemyWithTrait Ooze)) $ scenarioSpecific "devour" . toTarget
        selectEach (assetAt lid) $ scenarioSpecific "devour" . toTarget
        selectEach (eventAt lid) $ scenarioSpecific "devour" . toTarget
        selectEach (investigatorAt lid) $ scenarioSpecific "devour" . toTarget
        scenarioSpecific "devour" (toTarget lid)
      doStep (n - 1) msg'
      pure a
    _ -> TheAnomalySwells <$> liftRunMessage msg attrs

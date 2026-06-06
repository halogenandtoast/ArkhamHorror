module Arkham.Agenda.Cards.TheAnomalySpreads (theAnomalySpreads) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers (canDevourLocation)
import Arkham.Trait (Trait (Ooze, Oozified))

newtype TheAnomalySpreads = TheAnomalySpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAnomalySpreads :: AgendaCard TheAnomalySpreads
theAnomalySpreads = agenda (1, A) TheAnomalySpreads Cards.theAnomalySpreads (Static 6)

instance RunMessage TheAnomalySpreads where
  runMessage msg a@(TheAnomalySpreads attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      doStep 2 msg
      shuffleSetAsideIntoEncounterDeck $ mapOneOf cardIs [Enemies.cubicOoze, Enemies.graspingOoze]
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
    _ -> TheAnomalySpreads <$> liftRunMessage msg attrs

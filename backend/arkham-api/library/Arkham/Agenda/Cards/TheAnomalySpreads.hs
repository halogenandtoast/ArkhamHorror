module Arkham.Agenda.Cards.TheAnomalySpreads (theAnomalySpreads) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ooze, Oozified))

newtype TheAnomalySpreads = TheAnomalySpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Choose 2 Oozified locations, other than The Crater, without a copy of Vulnerable Heart. Subject 8L-08 devours both of those locations, along with all non-Ooze cards at those locations (Ooze enemies are instead discarded).
-- Shuffle 1 set-aside copy of Cubic Ooze and 1 set-aside copy of Grasping Ooze into the encounter deck, along with the encounter discard pile.

theAnomalySpreads :: AgendaCard TheAnomalySpreads
theAnomalySpreads = agenda (1, A) TheAnomalySpreads Cards.theAnomalySpreads (Static 6)

instance RunMessage TheAnomalySpreads where
  runMessage msg a@(TheAnomalySpreads attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      locations <-
        select
          $ LocationWithTrait Oozified
          <> not_ (locationIs Locations.theCrater)
          <> not_ (LocationWithEnemy (enemyIs Enemies.vulnerableHeart))
      lead <- getLead
      chooseNM lead 2 $ targets locations \lid -> do
        selectEach (enemyAt lid <> EnemyWithTrait Ooze) (toDiscard attrs)
        selectEach (enemyAt lid <> not_ (EnemyWithTrait Ooze)) $ scenarioSpecific "devour" . toTarget
        selectEach (assetAt lid) $ scenarioSpecific "devour" . toTarget
        selectEach (eventAt lid) $ scenarioSpecific "devour" . toTarget
        selectEach (investigatorAt lid) $ scenarioSpecific "devour" . toTarget
        scenarioSpecific "devour" (toTarget lid)
      pure a
    _ -> TheAnomalySpreads <$> liftRunMessage msg attrs

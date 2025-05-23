module Arkham.Scenario.Scenarios.ReturnToTheDevourerBelow (returnToTheDevourerBelow) where

import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheDevourerBelow
import Arkham.Scenarios.TheDevourerBelow.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ReturnToTheDevourerBelow = ReturnToTheDevourerBelow TheDevourerBelow
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasChaosTokenValue)

returnToTheDevourerBelow :: Difficulty -> ReturnToTheDevourerBelow
returnToTheDevourerBelow difficulty =
  scenarioWith
    (ReturnToTheDevourerBelow . TheDevourerBelow)
    "50032"
    "The Devourer Below"
    difficulty
    [ "woods1     .     woods2"
    , "woods1 mainPath woods2"
    , "woods3 mainPath woods4"
    , "woods3 ritualSite woods4"
    , "   .   ritualSite   .  "
    ]
    (referenceL .~ "01142")

instance RunMessage ReturnToTheDevourerBelow where
  runMessage msg s@(ReturnToTheDevourerBelow theDevourerBelow'@(TheDevourerBelow attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheDevourerBelow . TheDevourerBelow)
        attrs
        (setIsReturnTo >> setupTheDevourerBelow attrs)
    CreateEnemy creation@(enemyCreationMethod -> SpawnAtLocation lid) | creation.cardCode == Enemies.umordhoth.cardCode -> do
      whenMatch lid (location_ "Ritual Site") do
        createTreacheryAt_ Treacheries.vaultOfEarthlyDemise (AttachedToEnemy creation.enemy)
      pure s
    _ -> ReturnToTheDevourerBelow <$> liftRunMessage msg theDevourerBelow'

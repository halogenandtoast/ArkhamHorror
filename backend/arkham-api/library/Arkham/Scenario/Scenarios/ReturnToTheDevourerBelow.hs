module Arkham.Scenario.Scenarios.ReturnToTheDevourerBelow (returnToTheDevourerBelow) where

import Arkham.Difficulty
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheDevourerBelow
import Arkham.Scenarios.TheDevourerBelow.Helpers

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
  runMessage msg (ReturnToTheDevourerBelow theDevourerBelow'@(TheDevourerBelow attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheDevourerBelow . TheDevourerBelow)
        attrs
        (setIsReturnTo >> setupTheDevourerBelow attrs)
    _ -> ReturnToTheDevourerBelow <$> liftRunMessage msg theDevourerBelow'

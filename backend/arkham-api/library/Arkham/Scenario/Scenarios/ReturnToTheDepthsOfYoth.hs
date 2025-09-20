module Arkham.Scenario.Scenarios.ReturnToTheDepthsOfYoth (returnToTheDepthsOfYoth) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheDepthsOfYoth
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype ReturnToTheDepthsOfYoth = ReturnToTheDepthsOfYoth TheDepthsOfYoth
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheDepthsOfYoth :: Difficulty -> ReturnToTheDepthsOfYoth
returnToTheDepthsOfYoth difficulty = scenario
  (ReturnToTheDepthsOfYoth . TheDepthsOfYoth)
  "53059"
  "Return to The Depths of Yoth"
  difficulty
  []

instance RunMessage ReturnToTheDepthsOfYoth where
  runMessage msg (ReturnToTheDepthsOfYoth theDepthsOfYoth'@(TheDepthsOfYoth attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheDepthsOfYoth . TheDepthsOfYoth)
        attrs
        (setIsReturnTo >> setupTheDepthsOfYoth attrs)
    _ -> ReturnToTheDepthsOfYoth <$> liftRunMessage msg theDepthsOfYoth'

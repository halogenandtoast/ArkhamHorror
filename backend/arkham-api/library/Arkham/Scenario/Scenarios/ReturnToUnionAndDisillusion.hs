module Arkham.Scenario.Scenarios.ReturnToUnionAndDisillusion (returnToUnionAndDisillusion) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.UnionAndDisillusion
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype ReturnToUnionAndDisillusion
  = ReturnToUnionAndDisillusion UnionAndDisillusion
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToUnionAndDisillusion :: Difficulty -> ReturnToUnionAndDisillusion
returnToUnionAndDisillusion difficulty =
  scenarioWith
    (ReturnToUnionAndDisillusion . UnionAndDisillusion)
    "54046"
    "Return to Union and Disillusion"
    difficulty
    [ ".              miskatonicRiver ."
    , "unvisitedIsle3 forbiddingShore unvisitedIsle4"
    , "unvisitedIsle1 .               unvisitedIsle2"
    , "unvisitedIsle5 theGeistTrap    unvisitedIsle6"
    ]
    (referenceL .~ "05238")

instance RunMessage ReturnToUnionAndDisillusion where
  runMessage msg (ReturnToUnionAndDisillusion unionAndDisillusion'@(UnionAndDisillusion attrs)) =
    runQueueT $ scenarioI18n $ case msg of
      Setup ->
        runScenarioSetup
          (ReturnToUnionAndDisillusion . UnionAndDisillusion)
          attrs
          (setIsReturnTo >> setupUnionAndDisillusion attrs)
      _ -> ReturnToUnionAndDisillusion <$> liftRunMessage msg unionAndDisillusion'

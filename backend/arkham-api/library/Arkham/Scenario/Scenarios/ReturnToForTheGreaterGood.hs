module Arkham.Scenario.Scenarios.ReturnToForTheGreaterGood (returnToForTheGreaterGood) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.ForTheGreaterGood
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype ReturnToForTheGreaterGood
  = ReturnToForTheGreaterGood ForTheGreaterGood
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToForTheGreaterGood :: Difficulty -> ReturnToForTheGreaterGood
returnToForTheGreaterGood difficulty =
  scenarioWith
    (ReturnToForTheGreaterGood . ForTheGreaterGood)
    "54042"
    "Return to For the Greater Good"
    difficulty
    [ ".       .      lodgeGates       .              ."
    , ".       lobby  .                lodgeCellar    ."
    , "library lounge hiddenPassageway lodgeCatacombs sanctumDoorway1"
    , ".       vault  sanctumDoorway3  innerSanctum   sanctumDoorway2"
    ]
    (referenceL .~ "05197")

instance RunMessage ReturnToForTheGreaterGood where
  runMessage msg (ReturnToForTheGreaterGood forTheGreaterGood'@(ForTheGreaterGood attrs)) =
    runQueueT $ scenarioI18n $ case msg of
      Setup ->
        runScenarioSetup
          (ReturnToForTheGreaterGood . ForTheGreaterGood)
          attrs
          (setIsReturnTo >> setupForTheGreaterGood attrs)
      _ -> ReturnToForTheGreaterGood <$> liftRunMessage msg forTheGreaterGood'

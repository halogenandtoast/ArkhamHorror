module Arkham.Scenario.Scenarios.ReturnToTheMidnightMasks where

import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheMidnightMasks
import Arkham.Scenarios.TheMidnightMasks.Helpers

newtype ReturnToTheMidnightMasks = ReturnToTheMidnightMasks TheMidnightMasks
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasChaosTokenValue)

returnToTheMidnightMasks :: Difficulty -> ReturnToTheMidnightMasks
returnToTheMidnightMasks difficulty =
  scenarioWith
    (ReturnToTheMidnightMasks . TheMidnightMasks)
    "50025"
    "Return to The Midnight Masks"
    difficulty
    [ "northside downtown easttown"
    , "miskatonicUniversity rivertown graveyard"
    , "stMarysHospital southside yourHouse"
    ]
    ((decksL .~ mapFromList [(CultistDeck, [])]) . (referenceL .~ "01120"))

instance RunMessage ReturnToTheMidnightMasks where
  runMessage msg (ReturnToTheMidnightMasks theMidnightMasks'@(TheMidnightMasks attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheMidnightMasks . TheMidnightMasks)
        attrs
        (setIsReturnTo >> setupTheMidnightMasks attrs)
    _ -> ReturnToTheMidnightMasks <$> lift (runMessage msg theMidnightMasks')

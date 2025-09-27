module Arkham.Scenario.Scenarios.ReturnToTheWagesOfSin (returnToTheWagesOfSin) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheWagesOfSin
import Arkham.Scenarios.TheWagesOfSin.Helpers

newtype ReturnToTheWagesOfSin
  = ReturnToTheWagesOfSin TheWagesOfSin
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToTheWagesOfSin :: Difficulty -> ReturnToTheWagesOfSin
returnToTheWagesOfSin difficulty =
  scenarioWith
    (ReturnToTheWagesOfSin . TheWagesOfSin)
    "54034"
    "Return to The Witching Hour"
    difficulty
    [ ".              theGallows    .             chapelAttic     ."
    , "hereticsGraves hauntedFields .             abandonedChapel chapelCrypt"
    , ".              .             hangmansBrook .               ."
    ]
    (referenceL .~ "05161")

instance RunMessage ReturnToTheWagesOfSin where
  runMessage msg (ReturnToTheWagesOfSin theWagesOfSin'@(TheWagesOfSin attrs)) =
    runQueueT $ scenarioI18n $ case msg of
      Setup ->
        runScenarioSetup
          (ReturnToTheWagesOfSin . TheWagesOfSin)
          attrs
          (setIsReturnTo >> setupTheWagesOfSin attrs)
      _ -> ReturnToTheWagesOfSin <$> liftRunMessage msg theWagesOfSin'

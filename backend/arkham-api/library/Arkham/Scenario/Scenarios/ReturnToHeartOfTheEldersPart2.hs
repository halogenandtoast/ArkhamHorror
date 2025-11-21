module Arkham.Scenario.Scenarios.ReturnToHeartOfTheEldersPart2 (returnToHeartOfTheEldersPart2) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.HeartOfTheElders
import Arkham.Scenarios.HeartOfTheElders.Helpers

newtype ReturnToHeartOfTheEldersPart2 = ReturnToHeartOfTheEldersPart2 HeartOfTheElders
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToHeartOfTheEldersPart2 :: Difficulty -> ReturnToHeartOfTheEldersPart2
returnToHeartOfTheEldersPart2 difficulty =
  scenarioWith
    (ReturnToHeartOfTheEldersPart2 . HeartOfTheElders . (`with` HeartOfTheEldersMetadata Two False))
    "53048"
    "Return to The Heart of the Elders"
    difficulty
    [ ".        .        circle    circle    .     ."
    , ".        .        circle    circle    .     ."
    , "square   square   diamond   diamond   moon  moon"
    , "square   square   diamond   diamond   moon  moon"
    , ".        triangle triangle  heart     heart ."
    , ".        triangle triangle  heart     heart ."
    , "squiggle squiggle hourglass hourglass t     t"
    , "squiggle squiggle hourglass hourglass t     t"
    , ".        .        equals    equals    .     ."
    , ".        .        equals    equals    .     ."
    ]
    (referenceL .~ "04205")

instance RunMessage ReturnToHeartOfTheEldersPart2 where
  runMessage msg (ReturnToHeartOfTheEldersPart2 heartOfTheElders'@(HeartOfTheElders (attrs `With` metadata))) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToHeartOfTheEldersPart2 . HeartOfTheElders . (`with` metadata))
        attrs
        (setIsReturnTo >> setupHeartOfTheElders metadata attrs)
    _ -> ReturnToHeartOfTheEldersPart2 <$> liftRunMessage msg heartOfTheElders'

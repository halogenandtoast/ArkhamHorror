module Arkham.Scenario.Scenarios.ReturnToHeartOfTheEldersPart1 (returnToHeartOfTheEldersPart1) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.HeartOfTheElders
import Arkham.Scenarios.HeartOfTheElders.Helpers

newtype ReturnToHeartOfTheEldersPart1 = ReturnToHeartOfTheEldersPart1 HeartOfTheElders
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToHeartOfTheEldersPart1 :: Difficulty -> ReturnToHeartOfTheEldersPart1
returnToHeartOfTheEldersPart1 difficulty =
  scenarioWith
    (ReturnToHeartOfTheEldersPart1 . HeartOfTheElders . (`with` HeartOfTheEldersMetadata One False))
    "53045"
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

instance RunMessage ReturnToHeartOfTheEldersPart1 where
  runMessage msg (ReturnToHeartOfTheEldersPart1 heartOfTheElders'@(HeartOfTheElders (attrs `With` metadata))) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToHeartOfTheEldersPart1 . HeartOfTheElders . (`with` metadata))
        attrs
        (setIsReturnTo >> setupHeartOfTheElders metadata attrs)
    _ -> ReturnToHeartOfTheEldersPart1 <$> liftRunMessage msg heartOfTheElders'

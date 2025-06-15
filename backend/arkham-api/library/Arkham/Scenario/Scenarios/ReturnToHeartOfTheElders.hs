module Arkham.Scenario.Scenarios.ReturnToHeartOfTheElders (returnToHeartOfTheElders) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.HeartOfTheElders
import Arkham.Scenarios.HeartOfTheElders.Helpers

newtype ReturnToHeartOfTheElders = ReturnToHeartOfTheElders HeartOfTheElders
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToHeartOfTheElders :: Difficulty -> ReturnToHeartOfTheElders
returnToHeartOfTheElders difficulty =
  scenarioWith
    (ReturnToHeartOfTheElders . HeartOfTheElders . (`with` HeartOfTheEldersMetadata One False))
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

instance RunMessage ReturnToHeartOfTheElders where
  runMessage msg (ReturnToHeartOfTheElders heartOfTheElders'@(HeartOfTheElders (attrs `With` metadata))) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToHeartOfTheElders . HeartOfTheElders . (`with` metadata))
        attrs
        (setIsReturnTo >> setupHeartOfTheElders metadata attrs)
    _ -> ReturnToHeartOfTheElders <$> liftRunMessage msg heartOfTheElders'

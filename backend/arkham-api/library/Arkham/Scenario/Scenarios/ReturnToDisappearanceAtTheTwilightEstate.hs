module Arkham.Scenario.Scenarios.ReturnToDisappearanceAtTheTwilightEstate (returnToDisappearanceAtTheTwilightEstate) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.DisappearanceAtTheTwilightEstate
import Arkham.Scenarios.DisappearanceAtTheTwilightEstate.Helpers

newtype ReturnToDisappearanceAtTheTwilightEstate
  = ReturnToDisappearanceAtTheTwilightEstate DisappearanceAtTheTwilightEstate
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToDisappearanceAtTheTwilightEstate :: Difficulty -> ReturnToDisappearanceAtTheTwilightEstate
returnToDisappearanceAtTheTwilightEstate difficulty =
  scenarioWith
    (ReturnToDisappearanceAtTheTwilightEstate . DisappearanceAtTheTwilightEstate)
    "54016"
    "Return to Disappearance at the Twilight Estate"
    difficulty
    [ ".             .          office         .             ."
    , "billiardsRoom trophyRoom victorianHalls masterBedroom balcony"
    , ".             .          entryHall      wineCellar    ."
    ]
    (referenceL .~ "05043")

instance RunMessage ReturnToDisappearanceAtTheTwilightEstate where
  runMessage msg ( ReturnToDisappearanceAtTheTwilightEstate
                     disappearanceAtTheTwilightEstate'@(DisappearanceAtTheTwilightEstate attrs)
                   ) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToDisappearanceAtTheTwilightEstate . DisappearanceAtTheTwilightEstate)
        attrs
        (setIsReturnTo >> setupDisappearanceAtTheTwilightEstate attrs)
    _ ->
      ReturnToDisappearanceAtTheTwilightEstate <$> liftRunMessage msg disappearanceAtTheTwilightEstate'

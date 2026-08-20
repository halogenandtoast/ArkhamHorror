module Arkham.Scenario.Scenarios.ReturnToTheCircleUndone.ReturnToAtDeathsDoorstep (returnToAtDeathsDoorstep) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheCircleUndone.AtDeathsDoorstep
import Arkham.Scenarios.TheCircleUndone.AtDeathsDoorstep.Helpers

newtype ReturnToAtDeathsDoorstep
  = ReturnToAtDeathsDoorstep AtDeathsDoorstep
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToAtDeathsDoorstep :: Difficulty -> ReturnToAtDeathsDoorstep
returnToAtDeathsDoorstep difficulty =
  scenarioWith
    (ReturnToAtDeathsDoorstep . AtDeathsDoorstep)
    "54024"
    "Return to At Death's Doorstep"
    difficulty
    [ ".             .          office         .             ."
    , "billiardsRoom trophyRoom victorianHalls masterBedroom balcony"
    , ".             .          entryHall      wineCellar    ."
    ]
    (referenceL .~ "05065")

instance RunMessage ReturnToAtDeathsDoorstep where
  runMessage msg (ReturnToAtDeathsDoorstep atDeathsDoorstep'@(AtDeathsDoorstep attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToAtDeathsDoorstep . AtDeathsDoorstep)
        attrs
        (setIsReturnTo >> setupAtDeathsDoorstep attrs)
    _ ->
      ReturnToAtDeathsDoorstep <$> liftRunMessage msg atDeathsDoorstep'

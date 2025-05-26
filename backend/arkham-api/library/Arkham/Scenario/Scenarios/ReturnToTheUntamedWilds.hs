module Arkham.Scenario.Scenarios.ReturnToTheUntamedWilds (returnToTheUntamedWilds) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheUntamedWilds
import Arkham.Scenarios.TheUntamedWilds.Helpers

newtype ReturnToTheUntamedWilds = ReturnToTheUntamedWilds TheUntamedWilds
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheUntamedWilds :: Difficulty -> ReturnToTheUntamedWilds
returnToTheUntamedWilds difficulty = scenarioWith
  (ReturnToTheUntamedWilds . TheUntamedWilds)
  "53016"
  "Return to The Untamed Wilds"
  difficulty
  [ ".               .             ruinsOfEztli   .               ."
  , ".               serpentsHaven ruinsOfEztli   circuitousTrail ."
  , "templeOfTheFang serpentsHaven riverCanyon    circuitousTrail overgrownRuins"
  , "templeOfTheFang pathOfThorns  riverCanyon    ropeBridge      overgrownRuins"
  , ".               pathOfThorns  expeditionCamp ropeBridge      ."
  , ".               .             expeditionCamp .               ."
  ]
  (referenceL .~ "04043")

instance RunMessage ReturnToTheUntamedWilds where
  runMessage msg (ReturnToTheUntamedWilds theUntamedWilds'@(TheUntamedWilds attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup (ReturnToTheUntamedWilds . TheUntamedWilds) attrs (setIsReturnTo >> setupTheUntamedWilds attrs)
    _ -> ReturnToTheUntamedWilds <$> liftRunMessage msg theUntamedWilds'

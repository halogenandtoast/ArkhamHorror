module Arkham.Scenario.Scenarios.ReturnToShatteredAeons (returnToShatteredAeons) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.ShatteredAeons
import Arkham.Scenarios.ShatteredAeons.Helpers

newtype ReturnToShatteredAeons = ReturnToShatteredAeons ShatteredAeons
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToShatteredAeons :: Difficulty -> ReturnToShatteredAeons
returnToShatteredAeons difficulty =
  scenarioWith
    (ReturnToShatteredAeons . ShatteredAeons)
    "53061"
    "Return to The City of Archives"
    difficulty
    [ "shoresOfRlyeh   betweenWorlds1 atlantis    ruinsOfNewYork      ."
    , "shoresOfRlyeh   betweenWorlds1 atlantis    ruinsOfNewYork      valusia"
    , "cityOfTheUnseen nexusOfNkai    .           aPocketInTime       valusia"
    , "cityOfTheUnseen nexusOfNlai    .           aPocketInTime       pnakotus"
    , "yuggoth         betweenWorlds2 mu          plateauOfLeng       pnakotus"
    , "yuggoth         betweenWorlds2 mu          plateauOfLeng       ."
    , ".               .              ultimaThule greatHallOfCeleano  buenosAires"
    , ".               .              ultimaThule greatHallOfCeleano  buenosAires"
    ]
    (referenceL .~ "04314")

instance RunMessage ReturnToShatteredAeons where
  runMessage msg (ReturnToShatteredAeons shatteredAeons'@(ShatteredAeons attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToShatteredAeons . ShatteredAeons)
        attrs
        (setIsReturnTo >> setupShatteredAeons attrs)
    _ -> ReturnToShatteredAeons <$> liftRunMessage msg shatteredAeons'

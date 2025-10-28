module Arkham.Scenario.Scenarios.ReturnToTheSecretName (returnToTheSecretName) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheSecretName
import Arkham.Scenarios.TheSecretName.Helpers

newtype ReturnToTheSecretName
  = ReturnToTheSecretName TheSecretName
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheSecretName :: Difficulty -> ReturnToTheSecretName
returnToTheSecretName difficulty =
  scenarioWith
    (ReturnToTheSecretName . TheSecretName)
    "54029"
    "Return to The Secret Name"
    difficulty
    [ ".              .                 .             unknownPlaces4           unknownPlaces1   unknownPlaces2         unknownPlaces3 ."
    , ".              walterGilmansRoom .             cityOfElderThings        physicsClassroom siteOfTheSacrifice     the9thWard     strangeGeometry1"
    , "decrepitDoor1  moldyHalls        decrepitDoor2 moldyHallsEarlierTonight keziahsRoom      witchHouseRuins        .              ."
    , ".              decrepitDoor3     .             salemGaol1692            twilightAbyss    courtOfTheGreatOldOnes .              strangeGeometry2"
    , ".              .                 libraryOfEbla templeOfRlyeh            unknownPlaces5   unknownPlaces6         unknownPlaces7 thePriceManor"
    ]
    (referenceL .~ "05120")

instance RunMessage ReturnToTheSecretName where
  runMessage msg (ReturnToTheSecretName theSecretName'@(TheSecretName attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheSecretName . TheSecretName)
        attrs
        (setIsReturnTo >> setupTheSecretName attrs)
    _ ->
      ReturnToTheSecretName <$> liftRunMessage msg theSecretName'

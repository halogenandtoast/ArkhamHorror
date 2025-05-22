module Arkham.Scenario.Scenarios.ReturnToDimCarcosa (returnToDimCarcosa) where

import Arkham.Scenario.Scenarios.DimCarcosa
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DimCarcosa.Helpers

newtype ReturnToDimCarcosa = ReturnToDimCarcosa DimCarcosa
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToDimCarcosa :: Difficulty -> ReturnToDimCarcosa
returnToDimCarcosa difficulty = scenarioWith
  (ReturnToDimCarcosa . DimCarcosa)
  "52059"
  "Return to Dim Carcosa"
  difficulty
  [ ".             darkSpires      ."
  , ".             depthsOfDemhe   recessesOfYourOwnMind"
  , "dimStreets    palaceOfTheKing ruinsOfCarcosa"
  , "theThroneRoom bleakPlains     stageOfTheWardTheatre"
  , ".             shoresOfHali    ."
  ]
  (referenceL .~ "03316")

instance RunMessage ReturnToDimCarcosa where
  runMessage msg (ReturnToDimCarcosa dimCarcosa'@(DimCarcosa attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToDimCarcosa . DimCarcosa) attrs $ setIsReturnTo >> setupDimCarcosa attrs
    _ -> ReturnToDimCarcosa <$> liftRunMessage msg dimCarcosa'

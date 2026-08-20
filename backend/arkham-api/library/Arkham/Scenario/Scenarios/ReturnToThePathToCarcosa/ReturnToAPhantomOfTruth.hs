module Arkham.Scenario.Scenarios.ReturnToThePathToCarcosa.ReturnToAPhantomOfTruth (returnToAPhantomOfTruth) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.ThePathToCarcosa.APhantomOfTruth
import Arkham.Scenarios.ThePathToCarcosa.APhantomOfTruth.Helpers

newtype ReturnToAPhantomOfTruth = ReturnToAPhantomOfTruth APhantomOfTruth
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToAPhantomOfTruth :: Difficulty -> ReturnToAPhantomOfTruth
returnToAPhantomOfTruth difficulty =
  scenarioWith
    (ReturnToAPhantomOfTruth . APhantomOfTruth)
    "52040"
    "Return to A Phantom of Truth"
    difficulty
    [ "grandGuignol .                   canalSaintMartin ."
    , "grandGuignol montmartre           canalSaintMartin pèreLachaiseCemetery"
    , "opéraGarnier montmartre           leMarais         pèreLachaiseCemetery"
    , "opéraGarnier .                   leMarais         ."
    , "gareDOrsay   gardensOfLuxembourg notreDame        ."
    , "gareDOrsay   gardensOfLuxembourg notreDame        ."
    , ".            montparnasse        .                ."
    , ".            montparnasse        .                ."
    ]
    (referenceL .~ "03200")

instance RunMessage ReturnToAPhantomOfTruth where
  runMessage msg (ReturnToAPhantomOfTruth aPhantomOfTruth'@(APhantomOfTruth attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToAPhantomOfTruth . APhantomOfTruth)
        attrs
        (setIsReturnTo >> setupAPhantomOfTruth attrs)
    _ -> ReturnToAPhantomOfTruth <$> liftRunMessage msg aPhantomOfTruth'

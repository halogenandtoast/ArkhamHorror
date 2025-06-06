module Arkham.Scenario.Scenarios.ReturnToTheDoomOfEztli (returnToTheDoomOfEztli) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheDoomOfEztli
import Arkham.Scenarios.TheDoomOfEztli.Helpers

newtype ReturnToTheDoomOfEztli = ReturnToTheDoomOfEztli TheDoomOfEztli
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheDoomOfEztli :: Difficulty -> ReturnToTheDoomOfEztli
returnToTheDoomOfEztli difficulty =
  scenarioWith
    (ReturnToTheDoomOfEztli . TheDoomOfEztli)
    "53017"
    "Return to The Doom of Eztli"
    difficulty
    [ ".        ancientHall   mosaicChamber    snakePit          .             .    .    .    ."
    , ".        ancientHall   mosaicChamber    snakePit          .             .    .    .    ."
    , "entryway sealedPassage .                .                 chamberOfTime .    .    .    ."
    , "entryway sealedPassage .                .                 chamberOfTime .    .    .    ."
    , ".        grandChamber  throneRoom       tombOfTheAncients .             .    .    .    ."
    , ".        grandChamber  throneRoom       tombOfTheAncients .             .    .    .    ."
    , "pos1     pos2          pos3             pos4              pos5          pos6 pos7 pos8 pos9"
    ]
    (referenceL .~ "04054")

instance RunMessage ReturnToTheDoomOfEztli where
  runMessage msg (ReturnToTheDoomOfEztli theDoomOfEztli'@(TheDoomOfEztli attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheDoomOfEztli . TheDoomOfEztli)
        attrs
        (setIsReturnTo >> setupTheDoomOfEztli attrs)
    _ -> ReturnToTheDoomOfEztli <$> liftRunMessage msg theDoomOfEztli'

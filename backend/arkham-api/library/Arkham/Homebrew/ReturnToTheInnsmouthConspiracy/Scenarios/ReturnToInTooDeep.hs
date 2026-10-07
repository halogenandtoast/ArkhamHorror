module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToInTooDeep (returnToInTooDeep) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Acts
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (officialSetup, scenarioI18n)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.InTooDeep

newtype ReturnToInTooDeep = ReturnToInTooDeep InTooDeep
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToInTooDeep :: Difficulty -> ReturnToInTooDeep
returnToInTooDeep difficulty =
  scenarioWith
    (ReturnToInTooDeep . InTooDeep)
    ":return-to-the-innsmouth-conspiracy:028"
    "Return to In Too Deep"
    difficulty
    []
    (referenceL .~ "07123")

instance RunMessage ReturnToInTooDeep where
  runMessage msg (ReturnToInTooDeep inner@(InTooDeep attrs)) =
    runQueueT $ scenarioI18n "returnToInTooDeep" $ case msg of
      Setup -> runScenarioSetup (ReturnToInTooDeep . InTooDeep) attrs do
        setIsReturnTo
        replaceSet Set.AgentsOfCthulhu Sets.StalkersOfCthulhu
        replaceSet Set.RisingTide Sets.RollingTide
        replaceSet Set.Syzygy Sets.Occultation
        substitute Acts.throughTheLabyrinth HBActs.throughTheLabyrinthV2

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          officialSetup "inTooDeep" do
            li.nested "placeLocations" do
              li "barriers"
              li "startAt"
            li.nested "placeKeys" do
              li "blackKey"
              li "otherKeys"
            li "outForBlood"
          li.returnTo "setAsideCards"
          li.returnTo "setAsideInnsmouthInfluence"
          officialSetup "inTooDeep" do
            li "angryMob"
            li.nested "floodTokens" do
              li "increaseFloodLevel"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToInTooDeep
        setupInTooDeep attrs

        setAside $ replicate 4 HBTreacheries.innsmouthInfluence
      _ -> ReturnToInTooDeep <$> liftRunMessage msg inner

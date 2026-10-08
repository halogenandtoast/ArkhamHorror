module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToTheLairOfDagon (returnToTheLairOfDagon) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Acts
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (
  officialSetup,
  scenarioI18n,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Investigator.Projection ()
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.TheLairOfDagon
import Arkham.Scenarios.TheInnsmouthConspiracy.TheLairOfDagon.Helpers (scenarioLayout)

newtype ReturnToTheLairOfDagon = ReturnToTheLairOfDagon TheLairOfDagon
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToTheLairOfDagon :: Difficulty -> ReturnToTheLairOfDagon
returnToTheLairOfDagon difficulty =
  scenarioWith
    (ReturnToTheLairOfDagon . TheLairOfDagon)
    ":return-to-the-innsmouth-conspiracy:043"
    "Return to TheLairOfDagon"
    difficulty
    scenarioLayout
    (referenceL .~ "07274")

instance RunMessage ReturnToTheLairOfDagon where
  runMessage msg (ReturnToTheLairOfDagon inner@(TheLairOfDagon attrs)) =
    runQueueT $ scenarioI18n "returnToTheLairOfDagon" $ case msg of
      Setup -> runScenarioSetup (ReturnToTheLairOfDagon . TheLairOfDagon) attrs do
        setIsReturnTo
        replaceSet Set.Syzygy Sets.Occultation
        replaceSet Set.LockedDoors Sets.BarricadedDoors
        substitute Acts.theSecondOath HBActs.theSecondOathV2
        encounterWithASecretCult <- hasMemory AnEncounterWithASecretCult
        aDecisionToStickTogether <- hasMemory ADecisionToStickTogether
        aJailbreak <- hasMemory AJailbreak
        memories <- getRecordSet MemoriesRecovered

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          officialSetup "theLairOfDagon" do
            li.nested "placeKeys" do
              li "faceupKeys"
              li "facedownKeys"
            li.nested "placeLocations" do
              li "startAt"
              li "setAsideOtherLocations"
          li.returnTo "tidalTunnels"
          li.returnTo "doorway"
          officialSetup "theLairOfDagon" $ li "setAsideCards"
          li.returnTo "setAsideStirring"
          officialSetup "theLairOfDagon" do
            li.nested "checkMemories" do
              li.validate (length memories <= 4) "fourOrFewer"
              li.validate (length memories >= 5 && length memories <= 7) "fiveToSeven"
              li.validate (length memories >= 8) "eightOrMore"
            li.validate aJailbreak "jailbreak"
            li.nested "checkSecretCult" do
              li.validate encounterWithASecretCult "theInitiationV1"
              li.validate (not encounterWithASecretCult) "theInitiationV2"
            li.nested "checkStickTogether" do
              li.validate aDecisionToStickTogether "whatLurksBelowV1"
              li.validate (not aDecisionToStickTogether) "whatLurksBelowV2"
            li "floodTokens"
          unscoped $ li "shuffleRemainder"

        {- "Replace one of each Tidal Pool, Underground River and Underwater Cavern from
        the original Flooded Caverns set with its counterpart from the Return to Flooded
        Caverns set." -}
        replaceOneOf Locations.tidalPool HBLocations.tidalPool
        replaceOneOf Locations.undergroundRiver HBLocations.undergroundRiver
        replaceOneOf Locations.underwaterCavern HBLocations.underwaterCavern
        gather Sets.ReturnToTheLairOfDagon
        gather Sets.ReturnToFloodedCaverns
        {- "Shuffle the three versions of Doorway to the Depths together, remove two of
        them from the game at random without looking." Declared rather than taken off the
        pile: the printed Doorway arrives with The Lair of Dagon, which the block below is
        what gathers, so it is not there yet to be removed. The survivor is set aside with
        the rest of the tunnels. -}
        let doorways =
              [ Locations.doorwayToTheDepths
              , HBLocations.doorwayToTheDepthsV2
              , HBLocations.doorwayToTheDepthsV3
              ]
        doorway <- sample (Locations.doorwayToTheDepths :| drop 1 doorways)
        excludeCards $ filter (/= doorway) doorways
        setupTheLairOfDagon attrs

        setAside [HBTreacheries.stirringInHisSleep]
      _ -> ReturnToTheLairOfDagon <$> liftRunMessage msg inner

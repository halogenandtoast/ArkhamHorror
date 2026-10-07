module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToIntoTheMaelstrom (returnToIntoTheMaelstrom) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Agendas
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Agendas qualified as HBAgendas
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (
  officialSetup,
  scenarioI18n,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.IntoTheMaelstrom

newtype ReturnToIntoTheMaelstrom = ReturnToIntoTheMaelstrom IntoTheMaelstrom
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToIntoTheMaelstrom :: Difficulty -> ReturnToIntoTheMaelstrom
returnToIntoTheMaelstrom difficulty =
  scenarioWith
    (ReturnToIntoTheMaelstrom . IntoTheMaelstrom)
    ":return-to-the-innsmouth-conspiracy:048"
    "Return to IntoTheMaelstrom"
    difficulty
    []
    (referenceL .~ "07311")

instance RunMessage ReturnToIntoTheMaelstrom where
  runMessage msg (ReturnToIntoTheMaelstrom inner@(IntoTheMaelstrom attrs)) =
    runQueueT $ scenarioI18n "returnToIntoTheMaelstrom" $ case msg of
      Setup -> runScenarioSetup (ReturnToIntoTheMaelstrom . IntoTheMaelstrom) attrs do
        setIsReturnTo
        replaceSet Set.Syzygy Sets.Occultation
        substitute Acts.backIntoTheDepths HBActs.backIntoTheDepthsV2
        substitute Agendas.underTheSurface HBAgendas.underTheSurfaceV2
        substitute Agendas.celestialAlignment HBAgendas.celestialAlignmentV2
        possessTheKey <- getHasRecord TheInvestigatorsPossessTheKeyToYhaNthlei
        possessAMap <- getHasRecord TheInvestigatorsPossessAMapOfYhaNthlei
        guardianDispatched <- getHasRecord TheGuardianOfYhanthleiIsDispatched
        recognized <- getHasRecord TheGatewayToYhanthleiRecognizesYouAsTheRightfulKeeper

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          officialSetup "intoTheMaelstrom" $ li.nested "placeKeys" do
            li.validate possessTheKey "blueKey"
            li.validate possessAMap "redKey"
            li.validate guardianDispatched "greenKey"
            li.validate recognized "yellowKey"
            li "fewerThanFour"
            li "shuffleKeys"
          li.returnTo "tidalTunnels"
          officialSetup "intoTheMaelstrom" do
            li.nested "placeLocations" do
              li "startAt"
              li "setAsideOtherLocations"
            li.nested "checkCampaignLog" do
              li "divingSuits"
              li "removeUnusedDivingSuits"
            li "actDeck"
            li "setAsideCards"
          li.returnTo "setAsideCards"
          officialSetup "intoTheMaelstrom" $ li "floodTokens"
          unscoped $ li "shuffleRemainder"

        {- "Replace one of each Tidal Pool, Underground River and Underwater Cavern from
        the original Flooded Caverns set with its counterpart from the Return to Flooded
        Caverns set." -}
        replaceOneOf Locations.tidalPool HBLocations.tidalPool
        replaceOneOf Locations.undergroundRiver HBLocations.undergroundRiver
        replaceOneOf Locations.underwaterCavern HBLocations.underwaterCavern
        gather Sets.ReturnToIntoTheMaelstrom
        gather Sets.ReturnToFloodedCaverns
        setupIntoTheMaelstrom attrs

        setAside
          [ HBTreacheries.stirringInTheirSleep
          , HBTreacheries.stirringInTheirSleep
          , HBTreacheries.presenceOfTheFather
          , HBTreacheries.presenceOfTheMother
          ]
      _ -> ReturnToIntoTheMaelstrom <$> liftRunMessage msg inner

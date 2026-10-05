module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToIntoTheMaelstrom (returnToIntoTheMaelstrom) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.AgentsOfHydra qualified as Enemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Agendas qualified as HBAgendas
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (
  combineTidalTunnels,
  officialSetup,
  scenarioI18n,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.I18n
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.IntoTheMaelstrom

newtype ReturnToIntoTheMaelstrom = ReturnToIntoTheMaelstrom IntoTheMaelstrom
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

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
        setUsesGrid

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

        gather Sets.ReturnToIntoTheMaelstrom
        gather Sets.ReturnToFloodedCaverns
        gather Set.IntoTheMaelstrom
        gather Set.AgentsOfHydra
        gather Set.CreaturesOfTheDeep
        gather Set.FloodedCaverns
        gather Set.ShatteredMemories
        gather Set.Syzygy
        gather Set.AncientEvils

        setAgendaDeck [Agendas.underTheSurface, Agendas.celestialAlignment, Agendas.theFlood]
        setActDeck [Acts.backIntoTheDepths, Acts.cityOfTheDeepV1]

        lead <- getLead
        investigators <- allInvestigators
        when possessTheKey do
          chooseOneM lead do
            withI18n $ keyVar "color" "blue" $ questionLabeled "chooseInvestigatorForKey"
            targets investigators (`placeKey` BlueKey)

        when possessAMap do
          chooseOneM lead do
            withI18n $ keyVar "color" "red" $ questionLabeled "chooseInvestigatorForKey"
            targets investigators (`placeKey` RedKey)

        when guardianDispatched do
          chooseOneM lead do
            withI18n $ keyVar "color" "green" $ questionLabeled "chooseInvestigatorForKey"
            targets investigators (`placeKey` GreenKey)

        when recognized do
          chooseOneM lead do
            withI18n $ keyVar "color" "yellow" $ questionLabeled "chooseInvestigatorForKey"
            targets investigators (`placeKey` YellowKey)

        let
          ks =
            [BlueKey | not possessTheKey]
              <> [RedKey | not possessAMap]
              <> [GreenKey | not guardianDispatched]
              <> [YellowKey | not recognized]
        otherKs <- shuffle [PurpleKey, WhiteKey, BlackKey]

        setAsideKeys . map UnrevealedKey =<< shuffle (take 4 $ ks <> otherKs)

        gatewayToYhanthlei <- placeInGrid (Pos 0 0) Locations.gatewayToYhanthlei
        tidalTunnels <- shuffle . combineTidalTunnels =<< amongGathered (CardWithTitle "Tidal Tunnel")

        for_
          ( zip
              [Pos (-1) (-1), Pos (-1) 0, Pos (-1) 1, Pos 0 (-1), Pos 0 1, Pos 1 (-1), Pos 1 0, Pos 1 1]
              tidalTunnels
          )
          (uncurry placeLocationInGrid_)

        selectEach (investigatorWithRecord PossessesADivingSuit) \iid -> do
          divingSuit <- genCard Assets.divingSuit
          createAssetAt_ divingSuit (InPlayArea iid)
        removeEvery [Assets.divingSuit]

        dagonIsAwake <- getHasRecord DagonHasAwakened

        setAside
          [ Enemies.lloigor
          , Enemies.aquaticAbomination
          , if dagonIsAwake
              then Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom
              else Enemies.dagonDeepInSlumberIntoTheMaelstrom
          , Enemies.hydraDeepInSlumber
          , Acts.cityOfTheDeepV2
          , Acts.cityOfTheDeepV3
          ]

        setAside =<< amongGathered #location
        setAside
          [ HBTreacheries.stirringInTheirSleep
          , HBTreacheries.stirringInTheirSleep
          , HBTreacheries.presenceOfTheFather
          , HBTreacheries.presenceOfTheMother
          ]
        startAt gatewayToYhanthlei
      _ -> ReturnToIntoTheMaelstrom <$> liftRunMessage msg inner

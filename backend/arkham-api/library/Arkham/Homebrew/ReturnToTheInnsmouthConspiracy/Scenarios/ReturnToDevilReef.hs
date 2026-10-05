module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToDevilReef (returnToDevilReef) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as HBAssets
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (
  combineTidalTunnels,
  officialSetup,
  scenarioI18n,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher hiding (assetAt)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.DevilReef

newtype ReturnToDevilReef = ReturnToDevilReef DevilReef
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToDevilReef :: Difficulty -> ReturnToDevilReef
returnToDevilReef difficulty =
  scenarioWith
    (ReturnToDevilReef . DevilReef)
    ":return-to-the-innsmouth-conspiracy:031"
    "Return to DevilReef"
    difficulty
    []
    (referenceL .~ "07163")

instance RunMessage ReturnToDevilReef where
  runMessage msg s'@(ReturnToDevilReef inner@(DevilReef attrs)) =
    runQueueT $ scenarioI18n "returnToDevilReef" $ case msg of
      Setup -> runScenarioSetup (ReturnToDevilReef . DevilReef) attrs do
        setIsReturnTo
        replaceSet Set.RisingTide Sets.RollingTide
        substitute Assets.fishingVessel HBAssets.fishingVesselV2
        aBattle <- hasMemory ABattleWithAHorrifyingDevil

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          officialSetup "devilReef" do
            li.nested "placeKeys" do
              li "faceupKeys"
              li "facedownKeys"
            li.nested "churningWaters" do
              li "fishingVessel"
              li "startInVessel"
            li "setAsideRelics"
          li.returnTo "devilReefLocations"
          officialSetup "devilReef" $ li.nested "unfathomableDepths" do
            li "removeThree"
            li "setAsideThree"
          li.returnTo "tidalTunnels"
          officialSetup "devilReef" do
            li.nested "tidalTunnelsDeck" do
              li "unrevealedSide"
              li "placeNearEncounterDeck"
            li.nested "checkCampaignLog" do
              li.validate aBattle "secretsOfTheSeaV1"
              li.validate (not aBattle) "secretsOfTheSeaV2"
            li "floodTokens"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToDevilReef
        gather Sets.ReturnToFloodedCaverns
        gather Set.DevilReef
        gather Set.AgentsOfHydra
        gather Set.CreaturesOfTheDeep
        gather Set.FloodedCaverns
        gather Set.Malfunction
        gather Set.RisingTide

        whenHasRecord TheMissionWasSuccessful do
          investigators <- allInvestigators
          thomasDawson <- genCard Assets.thomasDawsonSoldierInANewWar
          leadChooseOneM do
            questionLabeled "addThomasDawsonToHand"
            targets investigators (`addToHand` only thomasDawson)

        let agenda1 = if aBattle then Agendas.secretsOfTheSeaV1 else Agendas.secretsOfTheSeaV2

        setAgendaDeck [agenda1, Agendas.theDevilOfTheDepths]
        setActDeck [Acts.reefOfMysteries]

        setAsideKeys [PurpleKey, WhiteKey, BlackKey]
        setAsideKeys . map UnrevealedKey =<< shuffleM [YellowKey, GreenKey, RedKey, BlueKey]

        churningWaters <- placeInGrid (Pos 0 0) Locations.churningWaters
        push $ SetFloodLevel churningWaters FullyFlooded
        fishingVessel <- assetAt HBAssets.fishingVesselV2 churningWaters
        eachInvestigator \iid -> push $ PlaceInvestigator iid (InVehicle fishingVessel)
        reveal churningWaters

        setAside [Assets.wavewornIdol, Assets.awakenedMantle, Assets.headdressOfYhaNthlei]

        cyclopeanRuins <- pickFrom (Locations.cyclopeanRuins_176a, Locations.cyclopeanRuins_176b)
        deepOneGrotto <- pickFrom (Locations.deepOneGrotto_175a, Locations.deepOneGrotto_175b)
        templeOfTheUnion <- pickFrom (Locations.templeOfTheUnion_177a, Locations.templeOfTheUnion_177b)

        setAside [cyclopeanRuins, deepOneGrotto, templeOfTheUnion]

        -- "Shuffle Cave Mouth in with the rest of the Devil Reef locations. Do not remove
        -- any; use all six." The sixth seat mirrors the northern one.
        zipWithM_ placeInGrid [Pos 0 3, Pos 4 2, Pos (-4) 2, Pos 4 (-2), Pos (-4) (-2), Pos 0 (-3)]
          =<< shuffleM
            [ Locations.lonelyIsle
            , Locations.hiddenCove
            , Locations.wavewornIsland
            , Locations.saltMarshes
            , Locations.blackReef
            , HBLocations.caveMouth
            ]
        addExtraDeck TidalTunnelDeck
          =<< shuffle
          . combineTidalTunnels
          =<< amongGathered (CardWithTitle "Tidal Tunnel")
      {- "Scenario Interlude: A Bargain", reached from Shrine to Hydra's ability. Pay the
      price and Innsmouth Influence joins your deck -- which is what makes you a Deep One
      investigator for the rest of the campaign. -}
      Msg.ScenarioSpecific "aBargain" (maybeResult -> Just iid) -> do
        scope "aBargain" $ storyWithChooseOneM (setTitle "title" >> p "body") do
          labeled "payThePrice" do
            addCampaignCardToDeck iid ShuffleIn HBTreacheries.innsmouthInfluence
            selectForMaybeM (locationIs HBLocations.shrineToHydra) \lid -> do
              clues <- field LocationClues lid
              when (clues > 0) $ discoverAt NotInvestigate iid ScenarioSource clues lid
            gainResources iid ScenarioSource 5
          labeled "backOut" nothing
        pure s'
      _ -> ReturnToDevilReef <$> liftRunMessage msg inner

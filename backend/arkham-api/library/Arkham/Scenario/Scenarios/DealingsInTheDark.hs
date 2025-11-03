module Arkham.Scenario.Scenarios.DealingsInTheDark (dealingsInTheDark) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Story
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DealingsInTheDark.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Treacheries

newtype DealingsInTheDark = DealingsInTheDark ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dealingsInTheDark :: Difficulty -> DealingsInTheDark
dealingsInTheDark difficulty =
  scenario
    DealingsInTheDark
    "09566"
    "Dealings in the Dark"
    difficulty
    [ ".    circle ."
    , "plus .      square"
    , ".    moon   ."
    ]

instance HasChaosTokenValue DealingsInTheDark where
  getChaosTokenValue iid tokenFace (DealingsInTheDark attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DealingsInTheDark where
  runMessage msg s@(DealingsInTheDark attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      skeys <- getCampaignStoryCards
      let anyKeys = toList skeys & any (any \card -> card.kind == KeyType)
      storyWithChooseOneM'
        (setTitle "title" >> p "intro1Part1" >> p.validate anyKeys "key" >> p "intro1Part2")
        do
          labeled' "giveToEce" $ doStep 2 PreScenarioSetup
          labeled' "giveToEceLying" $ doStep 3 PreScenarioSetup
          labeled' "takeForOurselves" $ doStep 4 PreScenarioSetup
      setupKeys
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      record TheCellIsWorkingWithEce
      swapTokens ElderThing Tablet
      flavor $ setTitle "title" >> p "intro2"
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      record TheCellIsDecievingEce
      swapTokens Tablet ElderThing
      flavor $ setTitle "title" >> p "intro3"
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      record TheCellRefusedEcesOffer
      flavor $ setTitle "title" >> p "intro4"
      pure s
    Setup -> runScenarioSetup DealingsInTheDark attrs do
      n <- getTime
      workingWithEce <- getHasRecord TheCellIsWorkingWithEce
      deceivingEce <- getHasRecord TheCellIsDecievingEce
      setup $ ul do
        li "gatherSets"
        li "theUnveiling"
        li.nested "timePassed" do
          li.validate (n <= 10) "tenOrFewerTime"
          li.validate (n >= 11 && n <= 17) "elevenToSeventeenTime"
          li.validate (n >= 18 && n <= 24) "eighteenToTwentyFourTime"
          li.validate (n >= 25) "twentyFiveOrMoreTime"
        li.nested "placeLocations" do
          li "beginPlay"
        li.nested "checkCampaignLog" do
          li.validate (workingWithEce || deceivingEce) "eceInvolved"
          li.validate (not $ workingWithEce || deceivingEce) "eceNotInvolved"
        li "setAside"
        li "miniCards"
        unscoped $ li "shuffleRemainder"
        li "cultists"
        unscoped $ li "readyToBegin"

      setUsesGrid
      gather Set.DealingsInTheDark
      gather Set.AgentsOfYuggoth
      gather Set.DarkVeiling
      gather Set.AncientEvils
      gather Set.DarkCult
      gather Set.LockedDoors
      gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

      setAgendaDeck [Agendas.agentsOfTheDark]
      setActDeck [Acts.searchForTheManuscript, Acts.searchForTheTalisman]

      startAt =<< placeInGrid (Pos 0 0) Locations.hagiaSophia
      placeInGrid_ (Pos (-1) 1) Locations.istanbulUniversity
      placeInGrid_ (Pos (-1) (-1)) Locations.obeliskOfTheodosius
      placeInGrid_ (Pos (-2) 0) Locations.galata

      if workingWithEce || deceivingEce
        then do
          investigators <- allInvestigators
          eceSahin <- fromGathered1 Assets.eceSahinTheVermillionVeiledLady
          leadChooseOneM do
            questionLabeledCard eceSahin
            portraits investigators $ createAssetAt_ eceSahin . InPlayArea
        else removeEvery [Assets.eceSahinTheVermillionVeiledLady]

      setAside
        [ Agendas.theChase
        , Locations.galataDocks
        , Locations.grandBazaarPublicBaths
        , Locations.grandBazaarBusyWalkway
        , Locations.grandBazaarCrowdedShops
        , Locations.grandBazaarJewelersRoad
        , Locations.grandBazaarDarkenedAlley
        , Locations.grandBazaarRooftopAccess
        , Locations.grandBazaarMarbleFountain
        , Keys.theTwistedAntiprism
        , Enemies.umbralHarbinger
        , Enemies.emissaryFromYuggoth
        ]

      theUnveiling <- genCard Stories.theUnveiling
      push $ PlaceStory theUnveiling Global

      eachInvestigator \iid -> do
        discardUntilFirst iid ScenarioSource Deck.EncounterDeck (basic #cultist)
      shuffleEncounterDiscardBackIn
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      drawCard iid ec
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          n <- getCurrentActStep
          if n == 1 || n == 2
            then do
              markTime 1
              setBearer Keys.theTwistedAntiprism $ keyWithEnemy Enemies.theRedGlovedManShroudedInMystery
              resolutionWithXp "noResolution" $ allGainXp' attrs
              endOfScenario
            else do
              resolution "noResolutionAct3"
              push R6
        Resolution 1 -> do
          theCellRefusedEcesOffer <- getHasRecord TheCellRefusedEcesOffer
          resolutionFlavor do
            setTitle "resolution1.title"
            p "resolution1.body"
            ul do
              li.nested "checkCampaignLog" do
                li.validate theCellRefusedEcesOffer "resolution1.theCellRefusedEcesOffer"
                li.validate (not theCellRefusedEcesOffer) "resolution1.theCellIsWorkingWithEce"
          push $ if theCellRefusedEcesOffer then R2 else R3
        Resolution 2 -> do
          markTime 2
          resolutionWithXp "resolution2" $ allGainXp' attrs
          chooseBearer Keys.theTwistedAntiprism
          endOfScenario
        Resolution 3 -> do
          working <- getHasRecord TheCellIsWorkingWithEce
          resolutionFlavor do
            setTitle "resolution3.title"
            p "resolution3.body"
            ul do
              li.nested "checkCampaignLog" do
                li.validate working "resolution3.theCellIsWorkingWithEce"
                li.validate (not working) "resolution3.theCellIsDeceivingEce"
          push $ if working then R4 else R5
        Resolution 4 -> do
          markTime 3
          record EceTrustsTheCell
          setBearer Keys.theTwistedAntiprism $ keyWithEnemy Assets.eceSahinTheVermillionVeiledLady
          resolutionWithXp "resolution4" $ allGainXp' attrs
          endOfScenario
        Resolution 5 -> do
          markTime 3
          record EceDoesNotTrustTheCell
          resolutionWithXp "resolution5" $ allGainXp' attrs
          chooseBearer Keys.theTwistedAntiprism
          endOfScenario
        Resolution 6 -> do
          markTime 2
          setBearer Keys.theTwistedAntiprism $ keyWithEnemy Enemies.theRedGlovedManShroudedInMystery
          resolutionWithXp "resolution6" $ allGainXp' attrs
          endOfScenario
        _ -> error "Unknown resolution for Dead Heat"
      pure s
    _ -> DealingsInTheDark <$> liftRunMessage msg attrs

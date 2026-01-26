module Arkham.Scenario.Scenarios.PreludeWelcomeToHemlockVale (preludeWelcomeToHemlockVale) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.CampaignSteps hiding (PreludeWelcomeToHemlockVale)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.HasQueue (clearQueue)
import Arkham.Cost.Status qualified as Cost
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location (getCanMoveToLocations)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Playable (getPlayableCardsMatch)
import Arkham.Helpers.Query (getJustLocationByName, getPlayerCount)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Story.Cards qualified as Stories
import Arkham.Strategy
import Arkham.Window (defaultWindows)

newtype PreludeWelcomeToHemlockVale = PreludeWelcomeToHemlockVale ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preludeWelcomeToHemlockVale :: Difficulty -> PreludeWelcomeToHemlockVale
preludeWelcomeToHemlockVale difficulty =
  scenarioWith
    PreludeWelcomeToHemlockVale
    "10704"
    "The Vale"
    difficulty
    [ ".     triangle square"
    , "moon  triangle square"
    , "moon  diamond  star"
    , "heart diamond  star"
    , "heart circle   spade"
    , ".     circle   spade"
    ]
    (hasEncounterDeckL .~ False)

instance HasChaosTokenValue PreludeWelcomeToHemlockVale where
  getChaosTokenValue iid tokenFace (PreludeWelcomeToHemlockVale attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PreludeWelcomeToHemlockVale where
  runMessage msg s@(PreludeWelcomeToHemlockVale attrs) = runQueueT $ campaignI18n $ scope "prelude1" $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (h "title" >> p "intro1") do
        labeled' "survey" $ doStep 2 PreScenarioSetup
        labeled' "feast" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro2"
      addChaosToken Tablet
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro3"
      addChaosToken ElderThing
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro4"
      pure s
    Setup -> runScenarioSetup PreludeWelcomeToHemlockVale attrs do
      setup $ ul do
        li "gatherSets"
        li "dayOne"
        li.nested "placeLocations" do
          li "locationsNote"
          li "beginPlay"
        li "setOutOfPlay"
        li "placeDoom"
        li "codex"
        unscoped $ li "readyToBegin"

      scope "playingPreludes" $ flavor $ h "title" >> p "body"

      gather Set.TheFirstDay
      gather Set.DayOfRest
      gather Set.TheVale
      gatherAndSetAside Set.Residents

      setAgendaDeck [Agendas.welcomeToHemlockVale]
      setActDeck [Acts.dawnOfTheFirstDay]

      startAt =<< place Locations.theCrossroadsDay

      placeStory Stories.dayOne

      placeAll
        [ Locations.boardingHouseDay
        , Locations.hemlockChapelDay
        , Locations.theOldMillDay
        , Locations.theAtwoodHouseDay
        , Locations.tadsGeneralStoreDay
        , Locations.valeSchoolhouseDay
        , Locations.theCommonsDay
        ]
      n <- getPlayerCount
      placeDoomOnAgenda n
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      case n of
        1 -> do
          entry "motherRachel"
          cards <-
            getPlayableCardsMatch
              source
              iid
              Cost.PaidCost
              (defaultWindows iid)
              (card_ $ oneOf [#spell, #charm])
          chooseOrRunOneM iid do
            targets cards $ putCardIntoPlay iid
            unscoped skip_
        2 -> do
          entry "leahAtwood"
          incrementRecordCount LeahAtwoodRelationshipLevel 1
          cards <- getPlayableCardsMatch source iid Cost.PaidCost (defaultWindows iid) (card_ #tool)
          chooseOrRunOneM iid do
            targets cards $ putCardIntoPlay iid
            unscoped skip_
        3 -> do
          entry "simeonAtwood"
          incrementRecordCount SimeonAtwoodRelationshipLevel 1
          search
            iid
            source
            iid
            [fromTopOfDeck 9]
            (basic $ oneOf [#tactic, #trick])
            (AddFoundToHand iid 1)
        4 -> do
          entry "williamHemlock"
          incrementRecordCount WilliamHemlockRelationshipLevel 1
          search
            iid
            source
            iid
            [fromTopOfDeck 9]
            (basic $ oneOf [#tome, #talent])
            (AddFoundToHand iid 1)
        5 -> do
          entry "riverHawthorne"
          incrementRecordCount GideonMizrahRelationshipLevel 1
          gainResources iid source 3
        6 -> do
          entry "gideonMizrah"
          incrementRecordCount GideonMizrahRelationshipLevel 1
          drawCards iid source 3
        7 -> do
          entry "judithPark"
          incrementRecordCount JudithParkRelationshipLevel 1
          cards <- getPlayableCardsMatch source iid Cost.PaidCost (defaultWindows iid) (card_ #weapon)
          chooseOrRunOneM iid do
            targets cards $ putCardIntoPlay iid
            unscoped skip_
        8 -> do
          entry "theoPeters"
          incrementRecordCount TheoPetersRelationshipLevel 1
          chooseOneM iid $ unscoped do
            labeled' "move" do
              locations <- getCanMoveToLocations iid source
              chooseTargetM iid locations $ moveTo source iid
            skip_
        9 -> do
          entry "boardingHouse"
          drawOk <- can.draw.cards iid
          resourceOk <- can.gain.resources iid
          chooseOneM iid $ unscoped do
            countVar 1 do
              labeledValidate' drawOk "drawCards" $ drawCards iid source 1
              labeledValidate' resourceOk "gainResources" $ gainResources iid source 1
            unscoped skip_
          boardingHouse <- getJustLocationByName "Boarding House"
          createAssetAt_ Assets.riverHawthorneBigInNewYork (AtLocation boardingHouse)
        10 -> do
          entry "theCrossroads"
          drawOk <- can.draw.cards iid
          resourceOk <- can.gain.resources iid
          chooseOneM iid $ unscoped do
            countVar 1 do
              labeledValidate' drawOk "drawCards" $ drawCards iid source 1
              labeledValidate' resourceOk "gainResources" $ gainResources iid source 1
            unscoped skip_
          theCrossroads <- getJustLocationByName "The Crossroads"
          createAssetAt_ Assets.theoPetersJackOfAllTrades (AtLocation theCrossroads)
        11 -> do
          entry "hemlockChapel"
          drawOk <- can.draw.cards iid
          resourceOk <- can.gain.resources iid
          chooseOneM iid $ unscoped do
            countVar 1 do
              labeledValidate' drawOk "drawCards" $ drawCards iid source 1
              labeledValidate' resourceOk "gainResources" $ gainResources iid source 1
            unscoped skip_
          hemlockChapel <- getJustLocationByName "Hemlock Chapel"
          createAssetAt_ Assets.motherRachelKindlyMatron (AtLocation hemlockChapel)
        12 -> do
          entry "theOldMill"
          drawOk <- can.draw.cards iid
          resourceOk <- can.gain.resources iid
          chooseOneM iid $ unscoped do
            countVar 1 do
              labeledValidate' drawOk "drawCards" $ drawCards iid source 1
              labeledValidate' resourceOk "gainResources" $ gainResources iid source 1
            unscoped skip_
          theOldMill <- getJustLocationByName "The Old Mill"
          createAssetAt_ Assets.leahAtwoodTheValeCook (AtLocation theOldMill)
        13 -> do
          entry "theAtwoodHouse"
          drawOk <- can.draw.cards iid
          resourceOk <- can.gain.resources iid
          chooseOneM iid $ unscoped do
            countVar 1 do
              labeledValidate' drawOk "drawCards" $ drawCards iid source 1
              labeledValidate' resourceOk "gainResources" $ gainResources iid source 1
            unscoped skip_
          theAtwoodHouse <- getJustLocationByName "The Atwood House"
          createAssetAt_ Assets.simeonAtwoodDedicatedTroublemaker (AtLocation theAtwoodHouse)
        14 -> do
          entry "tadsGeneralStore"
          resources <- getSpendableResources iid
          when (resources > 0) do
            chooseOneM iid do
              labeled' "tadsGeneralStore.item" do
                gameModifier source iid (ScenarioModifier "codex14")
                spendResources iid 1
                search iid source iid [fromDeck] (basic #item) (PlayFoundNoCost iid 1)
              unscoped skip_
          unlessM (selectAny $ assetIs Assets.judithParkTheMuscle) do
            tadsGeneralStore <- getJustLocationByName "Tad's General Store"
            createAssetAt_ Assets.judithParkTheMuscle (AtLocation tadsGeneralStore)
        15 -> do
          entry "valeSchoolhouse"
          drawOk <- can.draw.cards iid
          resourceOk <- can.gain.resources iid
          chooseOneM iid $ unscoped do
            countVar 1 do
              labeledValidate' drawOk "drawCards" $ drawCards iid source 1
              labeledValidate' resourceOk "gainResources" $ gainResources iid source 1
            unscoped skip_
          valeSchoolhouse <- getJustLocationByName "Vale Schoolhouse"
          createAssetAt_ Assets.williamHemlockAspiringPoet (AtLocation valeSchoolhouse)
        16 -> do
          entry "theCommons"
          search iid source iid [fromTopOfDeck 9] (basic #ally) (AddFoundToHand iid 1)
          theCommons <- getJustLocationByName "The Commons"
          createAssetAt_ Assets.gideonMizrahSeasonedSailor (AtLocation theCommons)
        _ -> error "invalid codex entry"
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        Resolution 1 -> do
          resolution "resolution1"
          eachInvestigator \iid -> do
            assets <- select $ assetControlledBy iid
            chooseOrRunOneM iid do
              for_ (eachWithRest assets) \(asset, rest) ->
                targeting asset do
                  setupModifier ScenarioSource asset Persist
                  for_ rest $ toDiscard ScenarioSource
            handSize <- getHandSize iid
            cs <- fieldMap InvestigatorHand length iid
            when (cs > handSize) $ chooseAndDiscardCards iid ScenarioSource (cs - handSize)
            shuffleDiscardBackIn iid
            rs <- getStartingResources iid
            n <- field InvestigatorResources iid
            when (n > rs) $ loseResources iid ScenarioSource (n - rs)
          addChaosToken AutoFail
          keepCardCache
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    EndOfScenario _mNextCampaignStep -> do
      lift clearQueue
      -- we do not want to adjust the hand
      -- Lifted.eachInvestigator (`Lifted.forTarget` msg)
      forTarget GameTarget msg
      standalone <- getIsStandalone
      if standalone
        then push GameOver
        else do
          leadChooseOneM do
            questionLabeled' "survey"
            labeled' "writtenInRock" $ afterPrelude WrittenInRock
            labeled' "hemlockHouse" $ afterPrelude HemlockHouse
            labeled' "theSilentHeath" $ afterPrelude TheSilentHeath
            labeled' "theLostSister" $ afterPrelude TheLostSister
            labeled' "theThingInTheDepths" $ afterPrelude TheThingInTheDepths
      pure s
    _ -> PreludeWelcomeToHemlockVale <$> liftRunMessage msg attrs

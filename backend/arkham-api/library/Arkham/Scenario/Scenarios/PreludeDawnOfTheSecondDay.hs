module Arkham.Scenario.Scenarios.PreludeDawnOfTheSecondDay (preludeDawnOfTheSecondDay) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.CampaignSteps hiding (PreludeDawnOfTheSecondDay)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Classes.HasQueue (clearQueue)
import Arkham.Cost.Status qualified as Cost
import Arkham.Effect.Builder
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location (getCanMoveToLocations)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Playable (getPlayableCardsMatch)
import Arkham.Helpers.Query (getInvestigators, getJustLocationByName, getPlayerCount)
import Arkham.I18n
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
import Arkham.ScenarioLogKey
import Arkham.Skill.Cards qualified as Skills
import Arkham.Story.Cards qualified as Stories
import Arkham.Strategy
import Arkham.Window (defaultWindows)

newtype PreludeDawnOfTheSecondDay = PreludeDawnOfTheSecondDay ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preludeDawnOfTheSecondDay :: Difficulty -> PreludeDawnOfTheSecondDay
preludeDawnOfTheSecondDay difficulty =
  scenarioWith
    PreludeDawnOfTheSecondDay
    "10677a"
    "The Vale"
    difficulty
    [ ".     triangle square"
    , "moon  triangle square"
    , "moon  diamond  star"
    , "heart diamond  star"
    , "heart circle   spade"
    , ".     circle   spade"
    ]
    $ (hasEncounterDeckL .~ False)
    . (referenceL .~ "10704")

instance HasChaosTokenValue PreludeDawnOfTheSecondDay where
  getChaosTokenValue iid tokenFace (PreludeDawnOfTheSecondDay attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PreludeDawnOfTheSecondDay where
  runMessage msg s@(PreludeDawnOfTheSecondDay attrs) = runQueueT $ campaignI18n $ scope "prelude2" $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      (finishedTheirMeal, others) <-
        partitionM (`matches` investigatorWithRecord FinishedTheirMeal) =<< getInvestigators
      storyOnly finishedTheirMeal $ buildFlavor $ h "title" >> p "theHemlockCurse"
      for_ finishedTheirMeal \iid -> addCampaignCardToDeck iid ShuffleIn Skills.theHemlockCurse
      storyOnly others $ buildFlavor $ h "title" >> p "gnawingHunger"
      pure s
    Setup -> runScenarioSetup PreludeDawnOfTheSecondDay attrs do
      setup $ ul do
        li "gatherSets"
        li "dayTwo"
        li.nested "placeLocations" do
          li "startAt"
        li.nested "residents" do
          li "removeResidents"
          li "judithPark"
          li "setOutOfPlay"
        li "placeDoom"
        unscoped $ li "readyToBegin"

      gather Set.TheSecondDay
      gather Set.DayOfRain
      gather Set.TheVale
      gatherAndSetAside Set.Residents

      doStep 1 Setup

      setAgendaDeck [Agendas.darkClouds]
      setActDeck [Acts.dawnOfTheSecondDay]
      placeStory Stories.dayTwo

      startAt =<< place Locations.boardingHouseDay
      theCrossroads <- place Locations.theCrossroadsDay
      createAssetAt_ Assets.judithParkTheMuscle (AtLocation theCrossroads)
      placeAll
        [ Locations.hemlockChapelDay
        , Locations.theOldMillDay
        , Locations.theAtwoodHouseDay
        , Locations.tadsGeneralStoreDay
        , Locations.valeSchoolhouseDay
        , Locations.theCommonsDay
        ]
      n <- getPlayerCount
      placeDoomOnAgenda n
    DoStep 1 Setup -> do
      getCrossedOutResidents >>= traverse_ (obtainCard <=< fetchCard)

      pure s
    ResolveAmounts iid (getChoiceAmount "$actions" -> n) ScenarioTarget -> do
      boardingHouseInvestigators <- select $ InvestigatorAt $ locationIs Locations.boardingHouseDay
      let spendFrom 0 _ = pure ()
          spendFrom _ [] = pure ()
          spendFrom remaining (i : is) = do
            iActions <- field InvestigatorRemainingActions i
            let toSpend = min remaining iActions
            when (toSpend > 0) $ spendActions i ScenarioSource toSpend
            spendFrom (remaining - toSpend) is
      spendFrom n boardingHouseInvestigators
      doStep (n + 1) (ScenarioSpecific "codex" (toJSON (iid, ScenarioSource :: Source, 9 :: Int)))
      pure s
    DoStep k (ScenarioSpecific "codex" v) -> do
      let (iid, _source :: Source, n :: Int) = toResult v
      case n of
        9 -> scope "codex" $ scope "boardingHouse" do
          theOldMill <- getJustLocationByName "The Old Mill"
          theCommons <- getJustLocationByName "The Commons"
          theCrossroads <- getJustLocationByName "The Crossroads"
          simeon <- selectAny $ SetAsideCardMatch $ cardIs Assets.simeonAtwoodDedicatedTroublemaker
          gideon <- selectAny $ SetAsideCardMatch $ cardIs Assets.gideonMizrahSeasonedSailor
          storyWithChooseOneM' (setTitle "title" >> p.green "body") do
            labeled' "help" do
              iids <- select $ InvestigatorAt $ locationIs Locations.boardingHouseDay
              chooseOrRunOneM iid do
                targets iids \iid' -> moveTo ScenarioSource iid' theCrossroads
            labeledValidate' simeon "simeon" do
              createAssetAt_ Assets.simeonAtwoodDedicatedTroublemaker (AtLocation theOldMill)
            labeledValidate' gideon "gideon" do
              createAssetAt_ Assets.gideonMizrahSeasonedSailor (AtLocation theCommons)
          when (k > 1) $ doStep (k - 1) (ScenarioSpecific "codex" v)
        _ -> pure ()
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      case n of
        1 -> do
          let motherRachelEntry k = setTitle "title" >> compose.green (p "header" >> p k)
          scope "motherRachel" $ flavor $ motherRachelEntry "motherRachel1"
          increaseRelationshipLevel MotherRachel 1
          eachInvestigator \iid' -> gainXp iid' attrs (ikey "xp.motherRachel") 1

          intervened <- getHasRecord MotherRachelIntervened
          if intervened
            then do
              scope "motherRachel" $ flavor $ motherRachelEntry "motherRachel2"
              addCampaignCardToDeck iid ShuffleIn Assets.cornHuskDoll
            else do
              scope "motherRachel" $ flavor $ motherRachelEntry "motherRachel3"
              drawCards iid source 1
              cards <-
                getPlayableCardsMatch source iid Cost.PaidCost (defaultWindows iid) (card_ $ oneOf [#charm, #spell])
              chooseOrRunOneM iid do
                targets cards $ putCardIntoPlay iid
                unscoped skip_
        3 -> do
          hatchedAPlan <- getHasRecord SimeonHatchedAPlan
          scope "simeonAtwood" $ flavor do
            setTitle "title"
            compose.green do
              p "header"
              p.validate hatchedAPlan "hatchedAPlan"
              hr
              p.validate (not hatchedAPlan) "otherwise"

          unless hatchedAPlan do
            drawCards iid source 1
            search
              iid
              source
              iid
              [fromTopOfDeck 9]
              (basic $ oneOf [#tactic, #trick])
              (AddFoundToHand iid 1)
        4 -> do
          record WilliamTookHeart
          entry "williamHemlock"
          increaseRelationshipLevel WilliamHemlock 1
          decreaseRelationshipLevel RiverHawthorne 1
          eachInvestigator \iid' -> gainXp iid' attrs (ikey "xp.williamHemlock") 2
          search
            iid
            source
            iid
            [fromTopOfDeck 9]
            (basic $ oneOf [#tome, #talent])
            (AddFoundToHand iid 1)
        5 -> do
          record TheSchemeIsInMotion
          increaseRelationshipLevel RiverHawthorne 2
          decreaseRelationshipLevel WilliamHemlock 1
          eachInvestigator \iid' -> gainXp iid' attrs (ikey "xp.riverHawthorne") 2
          entry "riverHawthorne"
          gainResources iid source 3
        6 -> do
          let gideonEntry k = setTitle "title" >> compose.green (p "header" >> p k)
          scope "gideonMizrah" $ flavor $ gideonEntry "gideon1"
          increaseRelationshipLevel GideonMizrah 1
          eachInvestigator \iid' -> gainXp iid' attrs (ikey "xp.gideonMizrah") 1

          toldStory <- getHasRecord GideonToldTheStoryOfCaptainHemlock
          if toldStory
            then do
              record GideonToldTheTaleOfTheAnnabelleLee
              scope "gideonMizrah" $ flavor $ gideonEntry "gideon2"
            else scope "gideonMizrah" $ flavor $ gideonEntry "gideon3"

          drawCards iid source 3
        7 -> do
          let judithEntry k = setTitle "title" >> compose.green (p "header" >> p k)
          scope "judithPark" $ flavor $ judithEntry "judith1"

          savedYourAss <- getHasRecord JudithSavedYourAss
          if savedYourAss
            then do
              scope "judithPark" $ storyWithChooseOneM' (judithEntry "judith2") do
                labeled' "iCanHandleMyself" do
                  increaseRelationshipLevel JudithPark 1
                  flavor $ judithEntry "judith3"
                labeled' "thanksForSavingUs" do
                  decreaseRelationshipLevel JudithPark 1
                  flavor $ judithEntry "judith4"
              eachInvestigator \iid' -> gainXp iid' attrs (ikey "xp.judithPark") 1
            else do
              drawCards iid source 1
              cards <- getPlayableCardsMatch source iid Cost.PaidCost (defaultWindows iid) (card_ #weapon)
              chooseOrRunOneM iid do
                targets cards $ putCardIntoPlay iid
                unscoped skip_
        8 -> do
          theoDistractedTheBear <- getHasRecord TheoDistractedTheBear
          when theoDistractedTheBear do
            incrementRecordCount TheoPetersRelationshipLevel 1
            eachInvestigator \iid' -> gainXp iid' attrs (ikey "xp.theoPeters") 1
          scope "theoPeters" $ flavor do
            setTitle "title"
            compose.green do
              p "header"
              p.validate theoDistractedTheBear "distractedTheBear"
              hr
              p.validate (not theoDistractedTheBear) "otherwise"
          chooseOneM iid $ unscoped do
            labeled' "move" do
              locations <- getCanMoveToLocations iid source
              chooseTargetM iid locations $ moveTo source iid
            skip_
        9 -> do
          simeon <- selectAny $ SetAsideCardMatch $ cardIs Assets.simeonAtwoodDedicatedTroublemaker
          gideon <- selectAny $ SetAsideCardMatch $ cardIs Assets.gideonMizrahSeasonedSailor
          let optionCount = 1 + (if simeon then 1 else 0) + (if gideon then 1 else 0)
          boardingHouseInvestigators <- select $ InvestigatorAt $ locationIs Locations.boardingHouseDay
          totalActions <- sum <$> traverse (field InvestigatorRemainingActions) boardingHouseInvestigators
          let maxAdditional = min (optionCount - 1) totalActions
          if maxAdditional > 0
            then do
              entry "boardingHouse"
              chooseAmount' iid "additionalActions" "$actions" 0 maxAdditional attrs
            else doStep 1 (ScenarioSpecific "codex" v)
        10 -> do
          entry "theCrossroads"
          drawCards iid source 1
          remember YouAreRunningAnErrand
        11 -> do
          entry "hemlockChapel"
          hemlockChapel <- getJustLocationByName "Hemlock Chapel"
          createAssetAt_ Assets.motherRachelKindlyMatron (AtLocation hemlockChapel)
        12 -> do
          entry "theOldMill"
          drawCards iid source 3
        13 -> do
          william <- getRelationshipLevel WilliamHemlock
          river <- getRelationshipLevel RiverHawthorne
          theAtwoodHouse <- getJustLocationByName "The Atwood House"
          scope "theAtwoodHouse" $ storyWithChooseOneM' (setTitle "title" >> p.green "body") do
            labeledValidate' (william >= 2) "william" do
              createAssetAt_ Assets.williamHemlockAspiringPoet (AtLocation theAtwoodHouse)
            labeledValidate' (river >= 2) "river" do
              createAssetAt_ Assets.riverHawthorneBigInNewYork (AtLocation theAtwoodHouse)
            labeled' "fight" do
              decrementRecordCount WilliamHemlockRelationshipLevel 1
              decrementRecordCount RiverHawthorneRelationshipLevel 1
              flavor $ setTitle "title" >> p.green "body2"
        14 -> do
          runningAnErrand <- remembered YouAreRunningAnErrand
          when runningAnErrand do
            incrementRecordCount LeahAtwoodRelationshipLevel 1
            eachInvestigator \iid' -> gainXp iid' attrs (ikey "xp.tadsGeneralStore") 1
          scope "tadsGeneralStore" $ flavor do
            setTitle "title"
            compose.green do
              p "header"
              p.validate runningAnErrand "runningAnErrand"
              hr
              p.validate (not runningAnErrand) "otherwise"
          resources <- getSpendableResources iid
          unless runningAnErrand do
            when (resources >= 3) $ scope "tadsGeneralStore" do
              chooseOneM iid do
                labeled' "item" do
                  spendResources iid 3
                  search iid source iid [fromDeck] (basic #item) (PlayFoundNoCost iid 1)
                unscoped skip_

          eachInvestigator \iid' -> effectWithSource source iid' do
            apply $ ScenarioModifier "codex14"
            removeOn $ #remembered YouAreRunningAnErrand
        15 -> do
          entry "valeSchoolhouse"
          valeSchoolhouse <- getJustLocationByName "Vale Schoolhouse"
          createAssetAt_ Assets.theoPetersJackOfAllTrades (AtLocation valeSchoolhouse)
        16 -> do
          entry "theCommons"
          iids <- getInvestigators
          residents <- filterM (fmap (<= 2) . getRelationshipLevel) [WilliamHemlock ..]
          chooseOrRunOneM iid do
            unscoped skip_
            for_ iids \iid' -> do
              assets <-
                select $ oneOf [inDeckOf iid', inHandOf NotForPlay iid', inDiscardOf iid'] <> basic CardIsStoryAsset
              unless (null assets) do
                targeting iid' do
                  focusCards assets do
                    chooseOneM iid' do
                      targets assets \asset -> do
                        removeCardFromDeckForCampaign iid' asset
                        obtainCard asset
                        chooseOneM iid do
                          for_ residents \resident -> do
                            cardLabeled resident do
                              increaseRelationshipLevel resident 1
                              eachInvestigator \iid'' -> gainXp iid'' attrs (ikey "xp.theCommons") 1
        17 -> do
          entry "theCurse"
          gainXp iid attrs (ikey "xp.theCurse") 2
          cursed <- selectAny $ InDeckOf (InvestigatorWithId iid) <> basic (cardIs Skills.theHemlockCurse)
          if cursed
            then setupModifier CampaignSource iid (CampaignModifier "theCurse")
            else addCampaignCardToDeck iid ShuffleIn Skills.theHemlockCurse
        _ -> error "invalid codex entry"
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        Resolution 1 -> do
          resolution "resolution1"
          push R3
        Resolution 2 -> do
          resolution "resolution2"
          push R3
        Resolution 3 -> do
          resolution "resolution3"
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
          areas <- getAreasSurveyed
          let survey k = unless (k `elem` areas)
          leadChooseOneM do
            questionLabeled' "survey"
            survey NorthPointMine do
              scenarioLabeled' "writtenInRock" "10501-day2" $ afterPrelude WrittenInRock
            survey HemlockHarbor do
              scenarioLabeled' "hemlockHouse" "10523-day2" $ afterPrelude HemlockHouse
            survey PearlRidge do
              scenarioLabeled' "theSilentHeath" "10549-day2" $ afterPrelude TheSilentHeath
            survey AkwanShoreline do
              scenarioLabeled' "theLostSister" "10569-day2" $ afterPrelude TheLostSister
            survey EastwickBog do
              scenarioLabeled' "theThingInTheDepths" "10588-day2" $ afterPrelude TheThingInTheDepths
      pure s
    _ -> PreludeDawnOfTheSecondDay <$> liftRunMessage msg attrs

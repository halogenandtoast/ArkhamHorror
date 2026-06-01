module Arkham.Scenario.Scenarios.PreludeDawnOfTheFinalDay (preludeDawnOfTheFinalDay) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Calculation
import Arkham.Campaigns.TheFeastOfHemlockVale.CampaignSteps hiding (PreludeDawnOfTheFinalDay)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Classes.HasQueue (clearQueue)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Investigator (getHandSize, getStartingResources)
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards, randomDiscard)
import Arkham.Helpers.Query (getInvestigators, getJustLocationByName, getLead, getPlayerCount)
import Arkham.I18n
import Arkham.Id (InvestigatorId, PlayerId, getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (pattern PassedThisSkillTest)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (incrementRecordCount, record, remember, remembered)
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Stories
import Arkham.Strategy

newtype PreludeDawnOfTheFinalDay = PreludeDawnOfTheFinalDay ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preludeDawnOfTheFinalDay :: Difficulty -> PreludeDawnOfTheFinalDay
preludeDawnOfTheFinalDay difficulty =
  scenarioWith
    PreludeDawnOfTheFinalDay
    "10679a"
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

instance HasChaosTokenValue PreludeDawnOfTheFinalDay where
  getChaosTokenValue iid tokenFace (PreludeDawnOfTheFinalDay attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PreludeDawnOfTheFinalDay where
  runMessage msg s@(PreludeDawnOfTheFinalDay attrs) = runQueueT $ campaignI18n $ scope "prelude3" $ case msg of
    PreScenarioSetup -> scope "intro" do
      hasPlan <- getHasRecord DrMarquezHasAPlan
      -- Check your Campaign Log. If Dr. Marquez has a plan, proceed to Intro 1,
      -- otherwise skip to Intro 2.
      flavor do
        h "title"
        p "checkCampaignLog"
        ul do
          li.validate hasPlan "proceedToIntro1"
          li.validate (not hasPlan) "skipToIntro2"
      if hasPlan
        then flavor $ h "title" >> p "intro1"
        else flavor $ h "title" >> p "intro2"
      flavor $ h "title" >> p "intro3"
      -- More survey assistance has arrived. Any player whose investigator was
      -- killed in The Longest Night may select a new investigator and upgrade
      -- their new deck to half the earned experience of their previous
      -- investigator (rounded up). The campaign transition into this prelude is
      -- set up to defer the killed/insane handling to this point (see
      -- Arkham.Campaign.Campaigns.TheFeastOfHemlockVale), so we trigger the
      -- new-investigator selection here and then award the half experience.
      killed <- select KilledInvestigator
      halfXp <- for killed \iid -> do
        pid <- getPlayer iid
        xp <- field InvestigatorXp iid
        pure (pid, (xp + 1) `div` 2)
      unless (null halfXp) do
        push $ Msg.chooseUpgradeDecks (map fst halfXp)
        doStep 1 (ScenarioSpecific "survivorXp" (toJSON halfXp))
      -- The fatigue from the long night catches up to you.
      replaceFatigueChaosTokens
      pure s
    DoStep 1 (ScenarioSpecific "survivorXp" v) -> do
      let halfXp = toResult v :: [(PlayerId, Int)]
      for_ halfXp \(pid, n) -> when (n > 0) do
        selectOne (InvestigatorIsPlayer pid) >>= traverse_ \iid ->
          gainXp iid ScenarioSource (ikey "xp.survivor") n
      pure s
    Setup -> runScenarioSetup PreludeDawnOfTheFinalDay attrs do
      setup $ ul do
        li "gatherSets"
        li "dayThree"
        li "buildActAgenda"
        li.nested "placeLocations" do
          li "crossroadsAndOldMill"
          li "startAt"
        li.nested "residents" do
          li "removeResidents"
          li "motherRachelAndJudith"
          li "setOutOfPlay"
        li "setAsideAgents"
        li "placeDoom"
        unscoped $ li "readyToBegin"

      gather Set.TheFinalDay
      gather Set.DayOfTheFeast
      gatherAndSetAside Set.AgentsOfTheColour
      gather Set.TheVale
      gatherAndSetAside Set.Residents

      setAgendaDeck [Agendas.allIsFullOfLove]
      setActDeck [Acts.dawnOfTheFinalDay]
      placeStory Stories.dayThree

      startAt =<< place Locations.boardingHouseDay
      theCrossroads <- place Locations.theCrossroadsMorning
      createAssetAt_ Assets.motherRachelKindlyMatron (AtLocation theCrossroads)
      createAssetAt_ Assets.judithParkTheMuscle (AtLocation theCrossroads)
      placeAll
        [ Locations.hemlockChapelDay
        , Locations.theOldMillMorning
        , Locations.theAtwoodHouseDay
        , Locations.tadsGeneralStoreDay
        , Locations.valeSchoolhouseDay
        , Locations.theCommonsDay
        ]

      doStep 1 Setup

      n <- getPlayerCount
      placeDoomOnAgenda n
    DoStep 1 Setup -> do
      getCrossedOutResidents >>= traverse_ (obtainCard <=< fetchCard)
      pure s
    -- Codex 9 (Boarding House): the group may spend any number of actions to
    -- unlock that many additional options. The chosen amount is spent across the
    -- investigators at the Boarding House, then we resolve that many extra picks.
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
      let (_iid :: InvestigatorId, _source :: Source, n :: Int) = toResult v
      case n of
        9 -> scope "codex" $ scope "boardingHouse" do
          theOldMill <- getJustLocationByName "The Old Mill"
          tadsStore <- getJustLocationByName "Tad's General Store"
          boardingHouse <- getJustLocationByName "Boarding House"
          william <- selectAny $ SetAsideCardMatch $ cardIs Assets.williamHemlockAspiringPoet
          theo <- selectAny $ SetAsideCardMatch $ cardIs Assets.theoPetersJackOfAllTrades
          river <- selectAny $ SetAsideCardMatch $ cardIs Assets.riverHawthorneBigInNewYork
          storyWithChooseOneM' (setTitle "title" >> p.green "body") do
            labeledValidate' william "william" do
              createAssetAt_ Assets.williamHemlockAspiringPoet (AtLocation theOldMill)
            labeledValidate' theo "theo" do
              createAssetAt_ Assets.theoPetersJackOfAllTrades (AtLocation tadsStore)
            labeledValidate' river "river" do
              createAssetAt_ Assets.riverHawthorneBigInNewYork (AtLocation boardingHouse)
          when (k > 1) $ doStep (k - 1) (ScenarioSpecific "codex" v)
        _ -> pure ()
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid, source :: Source, n :: Int) = toResult v
      -- Hiding fireworks (Codex 10/14/15): test Combat or Agility at the given
      -- difficulty. Each success marks 1 tally next to "The plan is underway"
      -- (handled in PassedThisSkillTest below).
      let hideFireworks skills x = do
            sid <- getRandom
            chooseBeginSkillTest sid iid attrs attrs skills (Fixed x)
      let drawOrResource = chooseOneM iid do
            labeled' "draw" $ drawCards iid source 1
            labeled' "gainResource" $ gainResources iid source 1
      case n of
        1 -> scope "motherRachel" do
          southernFields <- getHasRecord (AreasSurveyed SouthernFields)
          flavor do
            setTitle "title"
            compose.green do
              p "motherRachel1"
              ul do
                li.validate southernFields "proceedTo2"
                li.validate (not southernFields) "cannotReach"
          when southernFields do
            storyWithChooseOneM' (setTitle "title" >> p.green "motherRachel2") do
              labeled' "thankYou" do
                flavor $ setTitle "title" >> p.green "motherRachel3"
                record TheInvestigatorsLearnedTheirPlace
                increaseRelationshipLevel MotherRachel 1
                eachInvestigator \i -> gainXp i attrs (ikey "xp.motherRachel") 1
              labeled' "iSeeYou" do
                flavor $ setTitle "title" >> p.green "motherRachel4"
                decreaseRelationshipLevel MotherRachel 1
                eachInvestigator \i -> gainXp i attrs (ikey "xp.motherRachel") 1
        2 -> scope "leahAtwood" do
          simeonCrossedOut <- getHasRecord SimeonCrossedOut
          flavor do
            setTitle "title"
            compose.green do
              p.validate simeonCrossedOut "crossedOut"
              hr
              p.validate (not simeonCrossedOut) "notCrossedOut"
          let x = if simeonCrossedOut then 1 else 2
          increaseRelationshipLevel LeahAtwood x
          eachInvestigator \i -> gainXp i attrs (ikey "xp.leahAtwood") n
        4 -> scope "williamHemlock" do
          stood <- getHasRecord WilliamStoodByYou
          flavor do
            setTitle "title"
            compose.green do
              p "william1"
              ul do
                li.validate stood "proceedTo2"
                li.validate (not stood) "skipTo3"
          if stood
            then do
              tookHeart <- getHasRecord WilliamTookHeart
              flavor do
                setTitle "title"
                compose.green do
                  p "william2"
                  p.validate tookHeart "isResolved"
              when tookHeart $ record WilliamIsResolved
            else flavor $ setTitle "title" >> p.green "william3"
          increaseRelationshipLevel WilliamHemlock 1
          eachInvestigator \i -> gainXp i attrs (ikey "xp.williamHemlock") 1
        5 -> scope "riverHawthorne" do
          stood <- getHasRecord RiverStoodByYou
          scheme <- getHasRecord TheSchemeIsInMotion
          flavor do
            setTitle "title"
            compose.green do
              p.validate (stood && scheme) "stoodByYou"
              hr
              p.validate (not $ stood && scheme) "otherwise"
          if stood && scheme
            then do
              record RiverIsReclaimingTheirLegacy
              increaseRelationshipLevel RiverHawthorne 1
              eachInvestigator \i -> gainXp i attrs (ikey "xp.riverHawthorne") 1
            else do
              gainResources iid source 3
        6 -> scope "gideonMizrah" do
          stood <- getHasRecord GideonStoodByYou
          flavor do
            setTitle "title"
            compose.green do
              p.validate stood "stoodByYou"
              hr
              p.validate (not stood) "otherwise"
          if stood
            then do
              increaseRelationshipLevel GideonMizrah 1
              eachInvestigator \i -> gainXp i attrs (ikey "xp.gideonMizrah") 1
            else do
              drawCards iid source 3
        7 -> scope "judithPark" do
          theCrossroads <- getJustLocationByName "The Crossroads"
          createEnemyAt_ Enemies.miasmaticShadow theCrossroads
          stood <- getHasRecord JudithStoodByYou
          defeated <- getHasRecord TheThingInTheDepthsWasDefeated
          rel <- getRelationshipLevel JudithPark
          flavor do
            setTitle "title"
            compose.green do
              p "judith1"
              ul do
                li.validate (stood || defeated || rel >= 4) "proceedTo2"
                li.validate (not (stood || defeated || rel >= 4)) "skipTo3"
          if stood || defeated || rel >= 4
            then do
              flavor $ setTitle "title" >> p.green "judith2"
              judith <- selectJust $ assetIs Assets.judithParkTheMuscle
              takeControlOfAsset iid judith
              record YouBackedJudithUp
            else do
              flavor $ setTitle "title" >> p.green "judith3"
              randomDiscard iid source
          increaseRelationshipLevel JudithPark 1
          eachInvestigator \i -> gainXp i attrs (ikey "xp.judithPark") 1
        8 -> scope "theoPeters" do
          stood <- getHasRecord TheoStoodByYou
          flavor do
            setTitle "title"
            compose.green do
              p.validate stood "stoodByYou"
              hr
              p.validate (not stood) "otherwise"
          if stood
            then do
              record TheoIsHavingSecondThoughts
              increaseRelationshipLevel TheoPeters 1
              eachInvestigator \i -> gainXp i attrs (ikey "xp.theoPeters") 1
            else do
              remember YouAreDeliveringAPackage
        9 -> do
          william <- selectAny $ SetAsideCardMatch $ cardIs Assets.williamHemlockAspiringPoet
          theo <- selectAny $ SetAsideCardMatch $ cardIs Assets.theoPetersJackOfAllTrades
          river <- selectAny $ SetAsideCardMatch $ cardIs Assets.riverHawthorneBigInNewYork
          let optionCount = length $ filter id [william, theo, river]
          boardingHouseInvestigators <- select $ InvestigatorAt $ locationIs Locations.boardingHouseDay
          totalActions <- sum <$> traverse (field InvestigatorRemainingActions) boardingHouseInvestigators
          let maxAdditional = min (optionCount - 1) totalActions
          if maxAdditional > 0
            then do
              scope "boardingHouse" $ flavor $ setTitle "title" >> p.green "body"
              chooseAmount' iid "additionalActions" "$actions" 0 maxAdditional attrs
            else doStep 1 (ScenarioSpecific "codex" v)
        10 -> scope "theCrossroads" do
          planUnderway <- getHasRecord ThePlanIsUnderway
          flavor do
            setTitle "title"
            compose.green do
              p.validate planUnderway "planUnderway"
              hr
              p.validate (not planUnderway) "otherwise"
          if planUnderway
            then hideFireworks [#willpower, #combat] 3
            else drawOrResource
        11 -> scope "hemlockChapel" do
          planUnderway <- getHasRecord ThePlanIsUnderway
          gideonSetAside <- selectAny $ SetAsideCardMatch $ cardIs Assets.gideonMizrahSeasonedSailor
          flavor do
            setTitle "title"
            compose.green do
              p.validate planUnderway "planUnderway"
              hr
              p.validate (not planUnderway) "otherwise"

          when gideonSetAside do
            hemlockChapel <- getJustLocationByName "Hemlock Chapel"
            createAssetAt_ Assets.gideonMizrahSeasonedSailor (AtLocation hemlockChapel)
          when planUnderway $ hideFireworks [#agility] 2
        13 -> scope "theAtwoodHouse" do
          flavor $ setTitle "title" >> p.green "body"
          leahSetAside <- selectAny $ SetAsideCardMatch $ cardIs Assets.leahAtwoodTheValeCook
          when leahSetAside do
            theAtwoodHouse <- getJustLocationByName "The Atwood House"
            createAssetAt_ Assets.leahAtwoodTheValeCook (AtLocation theAtwoodHouse)
        14 -> scope "tadsGeneralStore" do
          planUnderway <- getHasRecord ThePlanIsUnderway
          flavor do
            setTitle "title"
            compose.green do
              p.validate planUnderway "planUnderway"
              hr
              p.validate (not planUnderway) "otherwise"
          if planUnderway
            then hideFireworks [#intellect, #agility] 3
            else do
              resources <- getSpendableResources iid
              when (resources >= 5) do
                chooseOneM iid do
                  labeled' "item" do
                    spendResources iid 5
                    search iid source iid [fromDeck] (basic #item) (PlayFoundNoCost iid 1)
                  unscoped skip_
        15 -> scope "valeSchoolhouse" do
          planUnderway <- getHasRecord ThePlanIsUnderway
          flavor do
            setTitle "title"
            compose.green do
              p.validate planUnderway "planUnderway"
              hr
              p.validate (not planUnderway) "otherwise"
          if planUnderway
            then hideFireworks [#combat, #agility] 3
            else drawOrResource
        16 -> scope "theCommons" do
          delivering <- remembered YouAreDeliveringAPackage
          flavor do
            setTitle "title"
            compose.green do
              p.validate delivering "delivering"
              hr
              p "regardless"
          when delivering do
            increaseRelationshipLevel TheoPeters 1
            eachInvestigator \i -> gainXp i attrs (ikey "xp.theoPeters") 1
          iids <- getInvestigators
          residents <- filterM (fmap (<= 2) . getRelationshipLevel) [WilliamHemlock ..]
          chooseOrRunOneM iid do
            unscoped skip_
            for_ iids \iid' -> do
              assets <-
                select
                  $ oneOf [inDeckOf iid', inHandOf NotForPlay iid', inDiscardOf iid']
                  <> basic (CardIsStoryAsset <> #item)
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
        _ -> error "invalid codex entry"
      pure s
    PassedThisSkillTest _iid (isSource attrs -> True) -> scope "codex" do
      -- A successfully hidden cluster of fireworks: mark 1 tally next to "The
      -- plan is underway" in Simeon Atwood's Notes.
      flavor $ p.green "fireworksHidden"
      incrementRecordCount ThePlanIsUnderway 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          lead <- getLead
          placeDoomOn ScenarioSource 1 lead
          push R2
        Resolution 1 -> do
          resolution "resolution1"
          record DrMarquesHasAHunch
          push R2
        Resolution 2 -> do
          resolution "resolution2"
          -- The Vale is full of fireworks if at least 1-per-investigator tallies
          -- were marked next to "The plan is underway" across the survey.
          fireworks <- getRecordCount ThePlanIsUnderway
          n <- getPlayerCount
          when (fireworks >= n) $ record TheValeIsFullOfFireworks
          -- Make preparations for the final survey: keep one non-starting asset,
          -- discard the rest, trim to opening hand size and starting resources.
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
            resources <- field InvestigatorResources iid
            when (resources > rs) $ loseResources iid ScenarioSource (resources - rs)
          keepCardCache
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    EndOfScenario _mNextCampaignStep -> do
      lift clearQueue
      -- we do not want to adjust the hand
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
              scenarioLabeled' "writtenInRock" "10501-day3" $ afterPrelude WrittenInRock
            survey HemlockHarbor do
              scenarioLabeled' "hemlockHouse" "10523-day3" $ afterPrelude HemlockHouse
            survey PearlRidge do
              scenarioLabeled' "theSilentHeath" "10549-day3" $ afterPrelude TheSilentHeath
            survey AkwanShoreline do
              scenarioLabeled' "theLostSister" "10569-day3" $ afterPrelude TheLostSister
            survey EastwickBog do
              scenarioLabeled' "theThingInTheDepths" "10588-day3" $ afterPrelude TheThingInTheDepths
      pure s
    _ -> PreludeDawnOfTheFinalDay <$> liftRunMessage msg attrs

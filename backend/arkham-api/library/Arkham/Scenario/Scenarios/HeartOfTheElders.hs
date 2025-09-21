module Arkham.Scenario.Scenarios.HeartOfTheElders (
  HeartOfTheEldersMetadata (..),
  HeartOfTheEldersScenarioStep (..),
  setupHeartOfTheElders,
  heartOfTheElders,
  HeartOfTheElders (..),
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.GameT (GameT)
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Helpers.Scenario qualified as Scenario
import Arkham.Helpers.Tokens
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Queue (QueueT)
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage)
import Arkham.Scenario.Types (
  Field (ScenarioVictoryDisplay),
  ScenarioAttrs (scenarioAdditionalReferences),
 )
import Arkham.Scenarios.HeartOfTheElders.Helpers
import Arkham.Token
import Arkham.Trait (Trait (Cave))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

data HeartOfTheEldersScenarioStep = One | Two
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HeartOfTheEldersMetadata = HeartOfTheEldersMetadata
  { scenarioStep :: HeartOfTheEldersScenarioStep
  , reachedAct2 :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HeartOfTheElders = HeartOfTheElders (ScenarioAttrs `With` HeartOfTheEldersMetadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heartOfTheElders :: Difficulty -> HeartOfTheElders
heartOfTheElders difficulty =
  scenario
    (HeartOfTheElders . (`with` HeartOfTheEldersMetadata One False))
    "04205"
    "Heart of the Elders"
    difficulty
    [ ".        .        circle    circle    .     ."
    , ".        .        circle    circle    .     ."
    , "square   square   diamond   diamond   moon  moon"
    , "square   square   diamond   diamond   moon  moon"
    , ".        triangle triangle  heart     heart ."
    , ".        triangle triangle  heart     heart ."
    , "squiggle squiggle hourglass hourglass t     t"
    , "squiggle squiggle hourglass hourglass t     t"
    , ".        .        equals    equals    .     ."
    , ".        .        equals    equals    .     ."
    ]

part2Locations :: [GridTemplateRow]
part2Locations =
  [ ".      diamond  .      ."
  , ".      diamond  moon   ."
  , "equals circle   moon   heart"
  , "equals circle   square heart"
  , ".      triangle square ."
  , ".      triangle .      ."
  ]

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance HasChaosTokenValue HeartOfTheElders where
  getChaosTokenValue iid chaosTokenFace (HeartOfTheElders (attrs `With` _)) =
    case chaosTokenFace of
      Skull -> do
        inCave <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Cave
        let caveModifier = if inCave then 2 else 0
        pure $ toChaosTokenValue attrs Skull (1 + caveModifier) (2 + caveModifier)
      Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
      Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
      ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
      otherFace -> getChaosTokenValue iid otherFace attrs

setupHeartOfTheElders
  :: ReverseQueue m => HeartOfTheEldersMetadata -> ScenarioAttrs -> ScenarioBuilderT m ()
setupHeartOfTheElders metadata attrs = scenarioI18n $ case scenarioStep metadata of
  One -> scope "part1" do
    pathsKnown <- getRecordCount PathsAreKnownToYou

    if pathsKnown == 6
      then do
        setup $ ul $ li.valid "pathsKnownToYou"
        push R1
      else do
        mappedOutTheWayForward <- getHasRecord TheInvestigatorsMappedOutTheWayForward
        setup do
          ul do
            li.invalid "pathsKnownToYou"
            li "gatherSets"
            li.nested "placeLocations" do
              li "insightIntoHowToEnterKnYan"
            li.validate (reachedAct2 metadata) "playedBefore"
            li "explorationDeck"
            li.nested "chooseLocations" do
              li.validate mappedOutTheWayForward "mappedOutTheWayForward"
            li "poisoned"
            unscoped $ li "shuffleRemainder"

        whenReturnTo do
          gather Set.ReturnToHeartOfTheElders
          gather Set.ReturnToPillarsOfJudgement
          gather Set.ReturnToRainforest
        gather Set.PillarsOfJudgement
        gather Set.HeartOfTheElders
        gather Set.Rainforest
        gather Set.Serpents
        gather Set.Expedition `orWhenReturnTo` gather Set.DoomedExpedition
        gather Set.Poison

        mouthOfKnYanTheCavernsMaw <- place Locations.mouthOfKnYanTheCavernsMaw
        startAt mouthOfKnYanTheCavernsMaw
        placeTokens attrs mouthOfKnYanTheCavernsMaw Pillar pathsKnown

        (ruinsLocation, toRemove) <-
          sampleWithRest $ Locations.overgrownRuins :| [Locations.templeOfTheFang, Locations.stoneAltar]

        removeOneOfEach toRemove

        when mappedOutTheWayForward $ place_ ruinsLocation

        square <- Locations.pathOfThorns `orSampleIfReturnTo` [Locations.riversideTemple]
        moon <- Locations.ropeBridge `orSampleIfReturnTo` [Locations.waterfall]
        triangle <- Locations.serpentsHaven `orSampleIfReturnTo` [Locations.trailOfTheDead]
        heart <- Locations.circuitousTrail `orSampleIfReturnTo` [Locations.cloudForest]

        isReturnTo <- getIsReturnTo
        let
          treacheries =
            guard (not isReturnTo)
              *> [ Treacheries.pitfall
                 , Treacheries.ants
                 , Treacheries.lostInTheWilds
                 , Treacheries.lowOnSupplies
                 ]

        addExtraDeck ExplorationDeck
          =<< shuffle
            ( [ruinsLocation | not mappedOutTheWayForward]
                <> [ Locations.timeWrackedWoods
                   , square
                   , Locations.riverCanyon
                   , moon
                   , triangle
                   , heart
                   ]
                <> treacheries
            )

        setAsidePoisonedCount <- getSetAsidePoisonedCount
        setAside $ replicate setAsidePoisonedCount Treacheries.poisoned

        when (reachedAct2 metadata) do
          enemyAt_ Enemies.theWingedSerpent mouthOfKnYanTheCavernsMaw

        let act1 =
              if isReturnTo
                then Acts.aFamiliarPattern
                else Acts.searchForThePattern

        setActDeck $ [act1 | not (reachedAct2 metadata)] <> [Acts.openingTheMaw]
        setAgendaDeck [Agendas.theJunglesHeart, Agendas.settingSun]

        whenReturnTo do
          setAside [Enemies.harbingerOfValusiaTheSleeperReturns]
          addAdditionalReferences ["53045b"]
          createAbilityEffect EffectGameWindow
            $ mkAbility (SourceableWithCardCode (CardCode "53045b") ScenarioSource) 1
            $ forced
            $ Explored #after Anyone Anywhere (SuccessfulExplore Anywhere)
  Two -> scope "part2" do
    setup do
      ul do
        li "gatherSets"
        li "placeLocations"
        li "theJungleWatches"
        li "setAside"
        li "explorationDeck"
        li "poisoned"
        unscoped $ li "shuffleRemainder"

    whenReturnTo do
      gather Set.ReturnToHeartOfTheElders
      gather Set.ReturnToKnYan
    gather Set.KnYan
    gather Set.HeartOfTheElders
    gather Set.AgentsOfYig
    gather Set.YigsVenom `orWhenReturnTo` gather Set.VenomousHate
    gather Set.ForgottenRuins
    gather Set.DeadlyTraps
    gather Set.Poison

    theJungleWatches <- recordedCardCodes <$> getRecordSet TheJungleWatches
    let theJungleWatchesCardDefs = mapMaybe lookupCardDef theJungleWatches

    when (notNull theJungleWatchesCardDefs) do
      placeInVictory theJungleWatchesCardDefs

    removeEvery [Locations.mouthOfKnYanTheCavernsMaw]
    startAt =<< place Locations.mouthOfKnYanTheDepthsBeneath

    setAsidePoisonedCount <- getSetAsidePoisonedCount
    setAside $ Locations.descentToYoth : replicate setAsidePoisonedCount Treacheries.poisoned

    isReturnTo <- getIsReturnTo

    let
      treacheries =
        guard (not isReturnTo)
          *> [ Treacheries.pitfall
             , Treacheries.noTurningBack
             , Treacheries.deepDark
             , Treacheries.finalMistake
             ]

    triangle <- Locations.darkHollow `orSampleIfReturnTo` [Locations.ruinsOfKnYan]
    square <- Locations.hallOfIdolatry `orSampleIfReturnTo` [Locations.chthonianDepths]
    diamond <- Locations.perilousGulch `orSampleIfReturnTo` [Locations.subterraneanSwamp]
    moon <- Locations.crystalPillars `orSampleIfReturnTo` [Locations.treacherousDescent]

    addExtraDeck ExplorationDeck
      =<< shuffle
        ( [ Locations.vastPassages
          , square
          , triangle
          , diamond
          , moon
          ]
            <> treacheries
        )

    setActDeck [Acts.cavernOfTheForgottenAge, Acts.descentIntoDark]
    setAgendaDeck [Agendas.theLonelyCaverns, Agendas.eyesInTheDark]

    setLayout part2Locations

    whenReturnTo do
      setAside [Enemies.harbingerOfValusiaTheSleeperReturns]
      addAdditionalReferences ["53045b"]
      createAbilityEffect EffectGameWindow
        $ mkAbility (SourceableWithCardCode (CardCode "53045b") ScenarioSource) 1
        $ forced
        $ Explored #after Anyone Anywhere (SuccessfulExplore Anywhere)

runAMessage :: Message -> HeartOfTheElders -> QueueT Message GameT HeartOfTheElders
runAMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = scenarioI18n $ scope "part1" $ case msg of
  StandaloneSetup -> scope "standalone" do
    lead <- getLead
    setChaosTokens standaloneChaosTokens

    chooseOneM lead do
      questionLabeled' "paths"
      for_ [0 .. 5] \n -> do
        labeled (tshow n) $ recordCount PathsAreKnownToYou n
    pure s
  PreScenarioSetup -> scope "intro" do
    storyWithChooseOneM' (h "title" >> p "intro1") do
      getOwner Assets.ichtacaTheForgottenGuardian >>= \case
        Nothing -> invalidLabeled' "ichtaca"
        Just iid ->
          labeled "ichtaca" do
            flavor $ h "title" >> p "intro2"
            putCampaignCardIntoPlay iid Assets.ichtacaTheForgottenGuardian

      getOwner Assets.alejandroVela >>= \case
        Nothing -> invalidLabeled' "alejandro"
        Just iid ->
          labeled "alejandro" do
            flavor $ h "title" >> p "intro3"
            putCampaignCardIntoPlay iid Assets.alejandroVela

      getOwner Assets.expeditionJournal >>= \case
        Nothing -> invalidLabeled' "expeditionJournal"
        Just iid ->
          labeled' "expeditionJournal" do
            flavor $ h "title" >> p "intro4"
            putCampaignCardIntoPlay iid Assets.expeditionJournal
      labeled' "else" nothing
    pure s
  ScenarioResolution r -> scope "resolutions" do
    case r of
      NoResolution -> do
        isReturnTo <- Scenario.getIsReturnTo
        if isReturnTo
          then resolutionWithChooseOne "noResolution" $ scope "noResolution" do
            labeled' "replay" $ do_ msg
            labeled' "resolution2" $ push R2
          else resolution "noResolution"
      Resolution 2 -> do
        resolution "resolution2"
        do_ R1
      _ -> do_ msg
    pure s
  Do (ScenarioResolution r) -> scope "resolutions" do
    case r of
      NoResolution -> do
        pathsKnown <- getRecordCount PathsAreKnownToYou
        pillarTokens <- selectCountTokens Pillar (locationIs Locations.mouthOfKnYanTheCavernsMaw)
        when (pillarTokens > pathsKnown) do
          recordCount PathsAreKnownToYou pillarTokens
        push RestartScenario
        actStep <- getCurrentActStep
        -- We need to clear out the additional references because they will stack up over time
        pure
          $ HeartOfTheElders
            ( attrs {scenarioAdditionalReferences = []}
                `With` metadata {reachedAct2 = reachedAct2 metadata || actStep >= 2}
            )
      Resolution 1 -> do
        resolutionWithXp "resolution1" (allGainXp' attrs)
        vengeanceCards <-
          filter (isJust . cdVengeancePoints . toCardDef)
            <$> scenarioField ScenarioVictoryDisplay
        recordSetInsert TheJungleWatches (map toCardCode vengeanceCards)
        allGainXp attrs
        push RestartScenario
        pure
          $ HeartOfTheElders (attrs {scenarioAdditionalReferences = []} `With` metadata {scenarioStep = Two})
      _ -> pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> liftRunMessage msg attrs

runBMessage :: Message -> HeartOfTheElders -> QueueT Message GameT HeartOfTheElders
runBMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = scenarioI18n $ scope "part2" $ case msg of
  ScenarioResolution r -> scope "resolutions" do
    case r of
      NoResolution -> do
        resolution "noResolution"
        rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
        push $ if rescuedAlejandro then R1 else R2
      Resolution n -> do
        let resolutionBody = if n == 1 then "resolution1" else "resolution2"

        resolutionWithXp resolutionBody (allGainXp' attrs)

        vengeance <- getTotalVengeanceInVictoryDisplay
        yigsFury <- getRecordCount YigsFury
        recordCount YigsFury (yigsFury + vengeance)

        inVictory <-
          selectAny
            $ VictoryDisplayCardMatch
            $ basic
            $ mapOneOf cardIs [Enemies.harbingerOfValusia, Enemies.harbingerOfValusiaTheSleeperReturns]
        if inVictory
          then crossOut TheHarbingerIsStillAlive
          else do
            damage <-
              selectOne
                (mapOneOf enemyIs [Enemies.harbingerOfValusia, Enemies.harbingerOfValusiaTheSleeperReturns]) >>= \case
                Just eid -> field EnemyDamage eid
                Nothing -> getRecordCount TheHarbingerIsStillAlive
            recordCount TheHarbingerIsStillAlive damage
        endOfScenario
    pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> liftRunMessage msg attrs

instance RunMessage HeartOfTheElders where
  runMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = runQueueT $ case msg of
    Setup ->
      runScenarioSetup (HeartOfTheElders . (`with` metadata)) attrs
        $ setupHeartOfTheElders metadata attrs
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ResolveChaosToken _ Tablet iid -> do
      whenPoisoned iid failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> withLocationOf iid \lid -> placeDoom Cultist lid 1
        Tablet | isHardExpert attrs -> unlessPoisoned iid do
          poison <- getSetAsidePoisoned
          push $ CreateWeaknessInThreatArea poison iid
        ElderThing -> assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      getEncounterDeck >>= \case
        Deck [] -> pure ()
        Deck (x : _) -> shuffleCardsIntoDeck ExplorationDeck [x]
      pure s
    _ -> case scenarioStep metadata of
      One -> runAMessage msg s
      Two -> runBMessage msg s

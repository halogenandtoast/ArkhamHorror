module Arkham.Scenario.Scenarios.HeartOfTheElders (
  HeartOfTheElders (..),
  heartOfTheElders,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.HeartOfTheElders.Story
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait (Trait (Cave))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

data ScenarioStep = One | Two
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Metadata = Metadata
  { scenarioStep :: ScenarioStep
  , reachedAct2 :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HeartOfTheElders = HeartOfTheElders (ScenarioAttrs `With` Metadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heartOfTheElders :: Difficulty -> HeartOfTheElders
heartOfTheElders difficulty =
  scenario
    (HeartOfTheElders . (`with` Metadata One False))
    "04205"
    "Heart of the Elders"
    difficulty
    [ ".               .               timeWrackedWoods timeWrackedWoods .               ."
    , ".               .               timeWrackedWoods timeWrackedWoods .               ."
    , "pathOfThorns    pathOfThorns    riverCanyon      riverCanyon      ropeBridge      ropeBridge"
    , "pathOfThorns    pathOfThorns    riverCanyon      riverCanyon      ropeBridge      ropeBridge"
    , ".               serpentsHaven   serpentsHaven    circuitousTrail  circuitousTrail ."
    , ".               serpentsHaven   serpentsHaven    circuitousTrail  circuitousTrail ."
    , "templeOfTheFang templeOfTheFang stoneAltar       stoneAltar       overgrownRuins overgrownRuins"
    , "templeOfTheFang templeOfTheFang stoneAltar       stoneAltar       overgrownRuins overgrownRuins"
    , ".               .               mouthOfKnYan     mouthOfKnYan     .              ."
    , ".               .               mouthOfKnYan     mouthOfKnYan     .              ."
    ]

part2Locations :: [GridTemplateRow]
part2Locations =
  [ ".            perilousGulch .              ."
  , ".            perilousGulch crystalPillars ."
  , "mouthOfKnYan vastPassages  crystalPillars descentToYoth"
  , "mouthOfKnYan vastPassages  hallOfIdolatry descentToYoth"
  , ".            darkHollow    hallOfIdolatry ."
  , ".            darkHollow    .              ."
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

runAMessage :: Runner HeartOfTheElders
runAMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
  StandaloneSetup -> do
    lead <- getLeadPlayer
    pushAll
      [ SetChaosTokens standaloneChaosTokens
      , questionLabel
          "The investigators may choose how many paths are known to you (choose a number between 0 and 5). The more paths are known to you, the quicker and easier the scenario will be."
          lead
          $ ChooseOne [Label (tshow n) [RecordCount PathsAreKnownToYou n] | n <- [0 .. 5]]
      ]
    pure s
  Setup -> do
    players <- allPlayers
    lead <- getLeadPlayer

    mIchtacaInvestigator <- getOwner Assets.ichtacaTheForgottenGuardian
    mAlejandroInvestigator <- getOwner Assets.alejandroVela
    mExpeditionJournalInvestigator <- getOwner Assets.expeditionJournal

    let
      introMessages =
        [ story players intro1
        , chooseOrRunOne lead
            $ [ Label
                "Let’s consult with Ichtaca."
                [ story players intro2
                , PutCampaignCardIntoPlay iid Assets.ichtacaTheForgottenGuardian
                ]
              | iid <- maybeToList mIchtacaInvestigator
              ]
            <> [ Label
                "Let’s consult with Alejandro."
                [ story players intro3
                , PutCampaignCardIntoPlay iid Assets.alejandroVela
                ]
               | iid <- maybeToList mAlejandroInvestigator
               ]
            <> [ Label
                "Let’s consult the expedition journal."
                [ story players intro4
                , PutCampaignCardIntoPlay iid Assets.expeditionJournal
                ]
               | iid <- maybeToList mExpeditionJournalInvestigator
               ]
            <> [Label "I wish we knew more about this..." []]
        ]

    pathsKnown <- getRecordCount PathsAreKnownToYou

    if pathsKnown == 6
      then do
        pushAll $ introMessages <> [ScenarioResolution $ Resolution 1]
        pure s
      else do
        let
          explorationDeckLocations =
            [ Locations.timeWrackedWoods
            , Locations.pathOfThorns
            , Locations.riverCanyon
            , Locations.ropeBridge
            , Locations.serpentsHaven
            , Locations.circuitousTrail
            ]
          ruinsLocations =
            Locations.overgrownRuins
              :| [Locations.templeOfTheFang, Locations.stoneAltar]
          explorationDeckTreacheries =
            [ Treacheries.pitfall
            , Treacheries.ants
            , Treacheries.lostInTheWilds
            , Treacheries.lowOnSupplies
            ]
        encounterDeck' <-
          buildEncounterDeckExcluding
            (explorationDeckLocations <> toList ruinsLocations)
            $ [ EncounterSet.PillarsOfJudgement
              , EncounterSet.HeartOfTheElders
              , EncounterSet.Rainforest
              , EncounterSet.Serpents
              , EncounterSet.Expedition
              , EncounterSet.Poison
              ]

        let
          encounterDeck =
            removeEachFromDeck encounterDeck' explorationDeckTreacheries

        (mouthOfKnYanTheCavernsMawId, placeMouthOfKnYanTheCavernsMaw) <-
          placeLocationCard Locations.mouthOfKnYanTheCavernsMaw
        theWingedSerpent <- genCard Enemies.theWingedSerpent
        ruinsLocation <- genCard =<< sample ruinsLocations
        placeRuinsLocation <- placeLocation_ ruinsLocation
        mappedOutTheWayForward <-
          getHasRecord
            TheInvestigatorsMappedOutTheWayForward

        explorationDeck <-
          shuffleM
            . (<> [ruinsLocation | not mappedOutTheWayForward])
            =<< genCards
              (explorationDeckLocations <> explorationDeckTreacheries)

        setAsidePoisonedCount <- getSetAsidePoisonedCount

        setAsideCards <-
          genCards
            (replicate setAsidePoisonedCount Treacheries.poisoned)

        createTheWingedSerpent <-
          createEnemyAt_
            theWingedSerpent
            mouthOfKnYanTheCavernsMawId
            Nothing

        pushAll
          $ introMessages
          <> [ SetEncounterDeck encounterDeck
             , SetAgendaDeck
             , SetActDeck
             , placeMouthOfKnYanTheCavernsMaw
             , MoveAllTo (toSource attrs) mouthOfKnYanTheCavernsMawId
             , PlaceTokens
                (toSource attrs)
                (LocationTarget mouthOfKnYanTheCavernsMawId)
                Resource
                pathsKnown
             ]
          <> [createTheWingedSerpent | reachedAct2 metadata]
          <> [placeRuinsLocation | mappedOutTheWayForward]

        acts <- genCards [Acts.searchForThePattern, Acts.openingTheMaw]
        agendas <- genCards [Agendas.theJunglesHeart, Agendas.settingSun]

        HeartOfTheElders
          . (`with` metadata)
          <$> runMessage
            msg
            ( attrs
                & (decksL . at ExplorationDeck ?~ explorationDeck)
                & (setAsideCardsL .~ setAsideCards)
                & (agendaStackL . at 1 ?~ agendas)
                & (actStackL . at 1 ?~ acts)
            )
  ScenarioResolution r -> case r of
    NoResolution -> do
      pathsKnown <- getRecordCount PathsAreKnownToYou
      pillarTokens <-
        getSum
          <$> selectAgg
            Sum
            LocationResources
            (locationIs Locations.mouthOfKnYanTheCavernsMaw)
      actStep <- getCurrentActStep
      pushAll
        $ [ RecordCount PathsAreKnownToYou pillarTokens
          | pillarTokens > pathsKnown
          ]
        <> [RestartScenario]
      pure
        $ HeartOfTheElders
          ( attrs
              `With` metadata
                { reachedAct2 = reachedAct2 metadata || actStep >= 2
                }
          )
    Resolution 1 -> do
      vengeanceCards <-
        filter (isJust . cdVengeancePoints . toCardDef)
          <$> scenarioField ScenarioVictoryDisplay
      gainXP <- toGainXp attrs getXp
      pushAll
        $ recordSetInsert TheJungleWatches (map toCardCode vengeanceCards)
        : gainXP
          <> [RestartScenario]
      pure $ HeartOfTheElders (attrs `With` metadata {scenarioStep = Two})
    _ -> pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> runMessage msg attrs

runBMessage :: Runner HeartOfTheElders
runBMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
  Setup -> do
    let
      explorationDeckLocations =
        [ Locations.vastPassages
        , Locations.hallOfIdolatry
        , Locations.darkHollow
        , Locations.perilousGulch
        , Locations.crystalPillars
        ]

      explorationDeckTreacheries =
        [ Treacheries.pitfall
        , Treacheries.noTurningBack
        , Treacheries.deepDark
        , Treacheries.finalMistake
        ]

    theJungleWatches <- recordedCardCodes <$> getRecordSet TheJungleWatches
    let theJungleWatchesCardDefs = mapMaybe (lookupCardDef) theJungleWatches
    theJungleWatchesCards <- genCards theJungleWatchesCardDefs

    encounterDeck' <-
      buildEncounterDeckExcluding
        explorationDeckLocations
        [ EncounterSet.KnYan
        , EncounterSet.HeartOfTheElders
        , EncounterSet.AgentsOfYig
        , EncounterSet.YigsVenom
        , EncounterSet.ForgottenRuins
        , EncounterSet.DeadlyTraps
        , EncounterSet.Poison
        ]

    let
      encounterDeck =
        removeEachFromDeck
          encounterDeck'
          (explorationDeckTreacheries <> theJungleWatchesCardDefs)

    (mouthOfKnYanTheDepthsBelowId, placeMouthOfKnYanTheDepthsBelow) <-
      placeLocationCard Locations.mouthOfKnYanTheDepthsBelow
    setAsidePoisonedCount <- getSetAsidePoisonedCount
    setAsideCards <-
      genCards
        ( Locations.descentToYoth
            : replicate setAsidePoisonedCount Treacheries.poisoned
        )

    explorationDeck <-
      genCards
        (explorationDeckLocations <> explorationDeckTreacheries)

    pushAll
      [ SetEncounterDeck encounterDeck
      , SetAgendaDeck
      , SetActDeck
      , placeMouthOfKnYanTheDepthsBelow
      , MoveAllTo (toSource attrs) mouthOfKnYanTheDepthsBelowId
      ]

    acts <-
      genCards
        [Acts.cavernOfTheForgottenAge, Acts.descentIntoDark]
    agendas <-
      genCards
        [Agendas.theLonelyCaverns, Agendas.eyesInTheDark]

    HeartOfTheElders
      . (`with` metadata)
      <$> runMessage
        msg
        ( attrs
            & (locationLayoutL .~ part2Locations)
            & (decksL . at ExplorationDeck ?~ explorationDeck)
            & (setAsideCardsL .~ setAsideCards)
            & (victoryDisplayL .~ theJungleWatchesCards)
            & (agendaStackL . at 1 ?~ agendas)
            & (actStackL . at 1 ?~ acts)
        )
  ScenarioResolution r -> do
    players <- allPlayers
    vengeance <- getVengeanceInVictoryDisplay
    yigsFury <- getRecordCount YigsFury
    inVictory <-
      selectAny
        $ VictoryDisplayCardMatch
        $ cardIs
          Enemies.harbingerOfValusia
    inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
    damage <- case inPlayHarbinger of
      Just eid -> field EnemyDamage eid
      Nothing -> getRecordCount TheHarbingerIsStillAlive
    case r of
      NoResolution -> do
        rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
        let n = if rescuedAlejandro then 1 else 2
        pushAll [story players noResolutionB, ScenarioResolution (Resolution n)]
      Resolution n -> do
        let
          resolutionStory = case n of
            1 -> resolution1B
            2 -> resolution2B
            _ -> error "invalid resolution"
        gainXp <- toGainXp attrs getXp
        pushAll
          $ story players resolutionStory
          : RecordCount YigsFury (yigsFury + vengeance)
          : [CrossOutRecord TheHarbingerIsStillAlive | inVictory]
            <> [RecordCount TheHarbingerIsStillAlive damage | not inVictory]
            <> gainXp
            <> [EndOfGame Nothing]
    pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> runMessage msg attrs

instance RunMessage HeartOfTheElders where
  runMessage msg s@(HeartOfTheElders (_ `With` metadata)) = case msg of
    Explore iid _ _ -> do
      windowMsg <- checkWindows [mkWindow Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    _ -> case scenarioStep metadata of
      One -> runAMessage msg s
      Two -> runBMessage msg s

module Arkham.Scenario.Scenarios.HeartOfTheElders
  ( HeartOfTheElders(..)
  , heartOfTheElders
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types ( Field (..) )
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message hiding (EnemyDamage)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.HeartOfTheElders.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait ( Trait (Cave) )
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
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
heartOfTheElders difficulty = scenario
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

standaloneTokens :: [TokenFace]
standaloneTokens =
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

instance HasTokenValue HeartOfTheElders where
  getTokenValue iid tokenFace (HeartOfTheElders (attrs `With` _)) =
    case tokenFace of
      Skull -> do
        inCave <-
          selectAny $ locationWithInvestigator iid <> LocationWithTrait Cave
        let caveModifier = if inCave then 2 else 0
        pure $ toTokenValue attrs Skull (1 + caveModifier) (2 + caveModifier)
      Cultist -> pure $ toTokenValue attrs Cultist 2 3
      Tablet -> pure $ toTokenValue attrs Tablet 2 3
      ElderThing -> pure $ toTokenValue attrs ElderThing 3 4
      otherFace -> getTokenValue iid otherFace attrs

runAMessage :: Message -> HeartOfTheElders -> GameT HeartOfTheElders
runAMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
  SetTokensForScenario -> do
    whenM getIsStandalone $ push $ SetTokens standaloneTokens
    pure s
  StandaloneSetup -> do
    leadInvestigatorId <- getLeadInvestigatorId
    push
      $ Ask leadInvestigatorId
      $ QuestionLabel
          "The investigators may choose how many paths are known to you (choose a number between 0 and 5). The more paths are known to you, the quicker and easier the scenario will be."
      $ ChooseOne
          [ Label (tshow n) [RecordCount PathsAreKnownToYou n] | n <- [0 .. 5] ]
    pure s
  Setup -> do
    iids <- getInvestigatorIds
    leadInvestigatorId <- getLeadInvestigatorId

    mIchtacaInvestigator <- getOwner Assets.ichtacaTheForgottenGuardian
    mAlejandroInvestigator <- getOwner Assets.alejandroVela
    mExpeditionJournalInvestigator <- getOwner Assets.expeditionJournal

    let
      introMessages =
        [ story iids intro1
        , chooseOrRunOne leadInvestigatorId
          $ [ Label
                "Let’s consult with Ichtaca."
                [ story iids intro2
                , PutCampaignCardIntoPlay iid Assets.ichtacaTheForgottenGuardian
                ]
            | iid <- maybeToList mIchtacaInvestigator
            ]
          <> [ Label
                 "Let’s consult with Alejandro."
                 [ story iids intro3
                 , PutCampaignCardIntoPlay iid Assets.alejandroVela
                 ]
             | iid <- maybeToList mAlejandroInvestigator
             ]
          <> [ Label
                 "Let’s consult the expedition journal."
                 [ story iids intro4
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

        mouthOfKnYanTheCavernsMaw <- genCard Locations.mouthOfKnYanTheCavernsMaw
        theWingedSerpent <- genCard Enemies.theWingedSerpent
        ruinsLocation <- genCard =<< sample ruinsLocations
        mappedOutTheWayForward <- getHasRecord
          TheInvestigatorsMappedOutTheWayForward

        explorationDeck <-
          shuffleM
          . (<> [ ruinsLocation | not mappedOutTheWayForward ])
          =<< traverse
                genCard
                (explorationDeckLocations <> explorationDeckTreacheries)

        setAsidePoisonedCount <- getSetAsidePoisonedCount

        setAsideCards <- traverse
          genCard
          (replicate setAsidePoisonedCount Treacheries.poisoned)

        pushAll
          $ introMessages
          <> [ SetEncounterDeck encounterDeck
             , SetAgendaDeck
             , SetActDeck
             , PlaceLocation mouthOfKnYanTheCavernsMaw
             , MoveAllTo
               (toSource attrs)
               (toLocationId mouthOfKnYanTheCavernsMaw)
             , PlaceResources
               (LocationTarget $ toLocationId mouthOfKnYanTheCavernsMaw)
               pathsKnown
             ]
          <> [ CreateEnemyAt
                 theWingedSerpent
                 (toLocationId mouthOfKnYanTheCavernsMaw)
                 Nothing
             | reachedAct2 metadata
             ]
          <> [ PlaceLocation ruinsLocation | mappedOutTheWayForward ]

        HeartOfTheElders . (`with` metadata) <$> runMessage
          msg
          (attrs
          & (decksL . at ExplorationDeck ?~ explorationDeck)
          & (setAsideCardsL .~ setAsideCards)
          & (agendaStackL
            . at 1
            ?~ [Agendas.theJunglesHeart, Agendas.settingSun]
            )
          & (actStackL
            . at 1
            ?~ [Acts.searchForThePattern, Acts.openingTheMaw]
            )
          )
  ScenarioResolution r -> case r of
    NoResolution -> do
      pathsKnown <- getRecordCount PathsAreKnownToYou
      pillarTokens <- getSum <$> selectAgg
        Sum
        LocationResources
        (locationIs Locations.mouthOfKnYanTheCavernsMaw)
      actStep <- getCurrentActStep
      pushAll
        $ [ RecordCount PathsAreKnownToYou pillarTokens
          | pillarTokens > pathsKnown
          ]
        <> [RestartScenario]
      pure $ HeartOfTheElders
        (attrs `With` metadata
          { reachedAct2 = reachedAct2 metadata || actStep >= 2
          }
        )
    Resolution 1 -> do
      vengeanceCards <- filter (isJust . cdVengeancePoints . toCardDef)
        <$> scenarioField ScenarioVictoryDisplay
      gainXP <- map (uncurry GainXP) <$> getXp
      pushAll
        $ RecordSet TheJungleWatches (map toCardCode vengeanceCards)
        : gainXP
        <> [RestartScenario]
      pure $ HeartOfTheElders (attrs `With` metadata { scenarioStep = Two })
    _ -> pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> runMessage msg attrs

runBMessage :: Message -> HeartOfTheElders -> GameT HeartOfTheElders
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
    theJungleWatchesCards <- for
      theJungleWatches
      (\cCode -> lookupCard cCode <$> getRandom)
    let theJungleWatchesCardDefs = map toCardDef theJungleWatchesCards

    encounterDeck' <- buildEncounterDeckExcluding
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
      encounterDeck = removeEachFromDeck
        encounterDeck'
        (explorationDeckTreacheries <> theJungleWatchesCardDefs)

    mouthOfKnYanTheDepthsBelow <- genCard Locations.mouthOfKnYanTheDepthsBelow
    setAsidePoisonedCount <- getSetAsidePoisonedCount
    setAsideCards <- traverse
      genCard
      (Locations.descentToYoth
      : replicate setAsidePoisonedCount Treacheries.poisoned
      )

    explorationDeck <- traverse
      genCard
      (explorationDeckLocations <> explorationDeckTreacheries)

    pushAll
      [ SetEncounterDeck encounterDeck
      , SetAgendaDeck
      , SetActDeck
      , PlaceLocation mouthOfKnYanTheDepthsBelow
      , MoveAllTo (toSource attrs) (toLocationId mouthOfKnYanTheDepthsBelow)
      ]

    HeartOfTheElders . (`with` metadata) <$> runMessage
      msg
      (attrs
      & (locationLayoutL .~ part2Locations)
      & (decksL . at ExplorationDeck ?~ explorationDeck)
      & (setAsideCardsL .~ setAsideCards)
      & (victoryDisplayL .~ theJungleWatchesCards)
      & (agendaStackL
        . at 1
        ?~ [Agendas.theLonelyCaverns, Agendas.eyesInTheDark]
        )
      & (actStackL
        . at 1
        ?~ [Acts.cavernOfTheForgottenAge, Acts.descentIntoDark]
        )
      )
  ScenarioResolution r -> do
    iids <- getInvestigatorIds
    vengeance <- getVengeanceInVictoryDisplay
    yigsFury <- getRecordCount YigsFury
    inVictory <- selectAny $ VictoryDisplayCardMatch $ cardIs
      Enemies.harbingerOfValusia
    inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
    damage <- case inPlayHarbinger of
      Just eid -> field EnemyDamage eid
      Nothing -> getRecordCount TheHarbingerIsStillAlive
    case r of
      NoResolution -> do
        rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
        let n = if rescuedAlejandro then 1 else 2
        pushAll [story iids noResolutionB, ScenarioResolution (Resolution n)]
      Resolution n -> do
        let
          resolutionStory = case n of
            1 -> resolution1B
            2 -> resolution2B
            _ -> error "invalid resolution"
        gainXp <- map (uncurry GainXP) <$> getXp
        pushAll
          $ [story iids resolutionStory]
          <> [RecordCount YigsFury (yigsFury + vengeance)]
          <> [ CrossOutRecord TheHarbingerIsStillAlive | inVictory ]
          <> [ RecordCount TheHarbingerIsStillAlive damage | not inVictory ]
          <> gainXp
          <> [EndOfGame Nothing]
    pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> runMessage msg attrs

instance RunMessage HeartOfTheElders where
  runMessage msg s@(HeartOfTheElders (_ `With` metadata)) = case msg of
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored
      pure s
    _ -> case scenarioStep metadata of
      One -> runAMessage msg s
      Two -> runBMessage msg s

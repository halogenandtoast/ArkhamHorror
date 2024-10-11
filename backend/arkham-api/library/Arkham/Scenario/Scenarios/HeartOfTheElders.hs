module Arkham.Scenario.Scenarios.HeartOfTheElders (HeartOfTheElders (..), heartOfTheElders) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.GameT (GameT)
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Queue (QueueT)
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage)
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.Scenarios.HeartOfTheElders.Story
import Arkham.Token
import Arkham.Trait (Trait (Cave))
import Arkham.Treachery.Cards qualified as Treacheries
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

runAMessage :: Message -> HeartOfTheElders -> QueueT Message GameT HeartOfTheElders
runAMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
  StandaloneSetup -> do
    lead <- getLead
    setChaosTokens standaloneChaosTokens

    chooseOneM lead do
      questionLabeled
        "The investigators may choose how many paths are known to you (choose a number between 0 and 5). The more paths are known to you, the quicker and easier the scenario will be."
      for_ [0 .. 5] \n -> do
        labeled (tshow n) $ recordCount PathsAreKnownToYou n
    pure s
  PreScenarioSetup -> do
    story intro1

    lead <- getLead
    chooseOneM lead do
      withOwner Assets.ichtacaTheForgottenGuardian \iid -> do
        labeled "Let’s consult with Ichtaca." do
          story intro2
          putCampaignCardIntoPlay iid Assets.ichtacaTheForgottenGuardian

      withOwner Assets.alejandroVela \iid -> do
        labeled "Let’s consult with Alejandro." do
          story intro3
          putCampaignCardIntoPlay iid Assets.alejandroVela

      withOwner Assets.expeditionJournal \iid -> do
        labeled "Let’s consult the expedition journal." do
          story intro4
          putCampaignCardIntoPlay iid Assets.expeditionJournal
      labeled "I wish we knew more about this..." nothing
    pure s
  Setup -> runScenarioSetup (HeartOfTheElders . (`with` metadata)) attrs do
    pathsKnown <- getRecordCount PathsAreKnownToYou

    if pathsKnown == 6
      then push R1
      else do
        gather Set.PillarsOfJudgement
        gather Set.HeartOfTheElders
        gather Set.Rainforest
        gather Set.Serpents
        gather Set.Expedition
        gather Set.Poison

        mouthOfKnYanTheCavernsMaw <- place Locations.mouthOfKnYanTheCavernsMaw
        startAt mouthOfKnYanTheCavernsMaw
        placeTokens attrs mouthOfKnYanTheCavernsMaw Resource pathsKnown

        (ruinsLocation, toRemove) <-
          sampleWithRest $ Locations.overgrownRuins :| [Locations.templeOfTheFang, Locations.stoneAltar]

        removeOneOfEach toRemove

        mappedOutTheWayForward <- getHasRecord TheInvestigatorsMappedOutTheWayForward
        when mappedOutTheWayForward $ place_ ruinsLocation

        addExtraDeck ExplorationDeck
          =<< shuffle
            ( [ruinsLocation | not mappedOutTheWayForward]
                <> [ Locations.timeWrackedWoods
                   , Locations.pathOfThorns
                   , Locations.riverCanyon
                   , Locations.ropeBridge
                   , Locations.serpentsHaven
                   , Locations.circuitousTrail
                   , Treacheries.pitfall
                   , Treacheries.ants
                   , Treacheries.lostInTheWilds
                   , Treacheries.lowOnSupplies
                   ]
            )

        setAsidePoisonedCount <- getSetAsidePoisonedCount
        setAside $ replicate setAsidePoisonedCount Treacheries.poisoned

        when (reachedAct2 metadata) do
          enemyAt_ Enemies.theWingedSerpent mouthOfKnYanTheCavernsMaw

        setActDeck $ [Acts.searchForThePattern | not (reachedAct2 metadata)] <> [Acts.openingTheMaw]
        setAgendaDeck [Agendas.theJunglesHeart, Agendas.settingSun]
  ScenarioResolution r -> case r of
    NoResolution -> do
      story noResolutionA
      pathsKnown <- getRecordCount PathsAreKnownToYou
      pillarTokens <-
        getSum <$> selectAgg Sum LocationResources (locationIs Locations.mouthOfKnYanTheCavernsMaw)
      actStep <- getCurrentActStep
      when (pillarTokens > pathsKnown) do
        recordCount PathsAreKnownToYou pillarTokens
      push RestartScenario
      pure $ HeartOfTheElders (attrs `With` metadata {reachedAct2 = reachedAct2 metadata || actStep >= 2})
    Resolution 1 -> do
      story resolution1A
      vengeanceCards <-
        filter (isJust . cdVengeancePoints . toCardDef)
          <$> scenarioField ScenarioVictoryDisplay
      recordSetInsert TheJungleWatches (map toCardCode vengeanceCards)
      allGainXp attrs
      push RestartScenario
      pure $ HeartOfTheElders (attrs `With` metadata {scenarioStep = Two})
    _ -> pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> liftRunMessage msg attrs

runBMessage :: Message -> HeartOfTheElders -> QueueT Message GameT HeartOfTheElders
runBMessage msg s@(HeartOfTheElders (attrs `With` metadata)) = case msg of
  Setup -> runScenarioSetup (HeartOfTheElders . (`with` metadata)) attrs do
    gather Set.KnYan
    gather Set.HeartOfTheElders
    gather Set.AgentsOfYig
    gather Set.YigsVenom
    gather Set.ForgottenRuins
    gather Set.DeadlyTraps
    gather Set.Poison

    theJungleWatches <- recordedCardCodes <$> getRecordSet TheJungleWatches
    let theJungleWatchesCardDefs = mapMaybe (lookupCardDef) theJungleWatches

    when (notNull theJungleWatchesCardDefs) do
      placeInVictory theJungleWatchesCardDefs

    startAt =<< place Locations.mouthOfKnYanTheDepthsBelow

    setAsidePoisonedCount <- getSetAsidePoisonedCount
    setAside $ Locations.descentToYoth : replicate setAsidePoisonedCount Treacheries.poisoned

    addExtraDeck ExplorationDeck
      =<< shuffle
        [ Locations.vastPassages
        , Locations.hallOfIdolatry
        , Locations.darkHollow
        , Locations.perilousGulch
        , Locations.crystalPillars
        , Treacheries.pitfall
        , Treacheries.noTurningBack
        , Treacheries.deepDark
        , Treacheries.finalMistake
        ]

    setActDeck [Acts.cavernOfTheForgottenAge, Acts.descentIntoDark]
    setAgendaDeck [Agendas.theLonelyCaverns, Agendas.eyesInTheDark]

    setLayout part2Locations
  ScenarioResolution r -> do
    case r of
      NoResolution -> do
        story noResolutionB
        rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
        push $ if rescuedAlejandro then R1 else R2
      Resolution n -> do
        story $ case n of
          1 -> resolution1B
          2 -> resolution2B
          _ -> error "invalid resolution"

        vengeance <- getVengeanceInVictoryDisplay
        yigsFury <- getRecordCount YigsFury
        recordCount YigsFury (yigsFury + vengeance)

        inVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.harbingerOfValusia
        if inVictory
          then crossOut TheHarbingerIsStillAlive
          else do
            damage <-
              selectOne (enemyIs Enemies.harbingerOfValusia) >>= \case
                Just eid -> field EnemyDamage eid
                Nothing -> getRecordCount TheHarbingerIsStillAlive
            recordCount TheHarbingerIsStillAlive damage
        allGainXp attrs
        endOfScenario
    pure s
  _ -> HeartOfTheElders . (`with` metadata) <$> liftRunMessage msg attrs

instance RunMessage HeartOfTheElders where
  runMessage msg s@(HeartOfTheElders (_ `With` metadata)) = runQueueT $ case msg of
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    _ -> case scenarioStep metadata of
      One -> runAMessage msg s
      Two -> runBMessage msg s

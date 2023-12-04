{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestImport (
  module X,
  module TestImport,
) where

import Arkham.Prelude as X hiding (assert)

import Arkham.Agenda as X
import Arkham.Asset as X (createAsset, lookupAsset)
import Arkham.Card as X
import Arkham.Card.EncounterCard as X
import Arkham.Card.PlayerCard as X
import Arkham.ChaosBag as X
import Arkham.ChaosToken as X
import Arkham.Classes as X hiding (getChaosTokenValue)
import Arkham.Cost as X hiding (PaidCost)
import Arkham.Enemy as X
import Arkham.Entities as X
import Arkham.Event as X
import Arkham.Game as X hiding (newGame, runMessages, withModifiers)
import Arkham.Game.Helpers as X hiding (getCanAffordCost)
import Arkham.Game.Utils as X hiding (getAsset)
import Arkham.GameValue as X
import Arkham.Helpers as X
import Arkham.Helpers.Message as X hiding (createEnemy, putCardIntoPlay)
import Arkham.Id as X
import Arkham.Investigator as X hiding (
  InvestigatorDamage,
  InvestigatorDefeated,
  InvestigatorResigned,
  allInvestigators,
  foundCardsL,
 )
import Arkham.Location as X
import Arkham.Scenario as X
import Arkham.SkillType as X
import Arkham.Source as X
import Arkham.Stats as X
import Arkham.Target as X
import Arkham.Window as X (
  Window (..),
  WindowType (DuringTurn, FastPlayerWindow, NonFast),
 )
import Control.Lens as X (set, (^?!))
import Control.Monad.Fail as X
import Control.Monad.State as X (get)
import Data.Maybe as X (fromJust)
import Data.UUID.V4 as X
import Helpers.Message as X hiding (playEvent)
import Test.Hspec as X

import Arkham.ActiveCost
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Cards.WhatsGoingOn
import Arkham.Agenda.Sequence
import Arkham.Agenda.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types
import Arkham.Classes.HasGame
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types
import Arkham.Entities qualified as Entities
import Arkham.Event.Types
import Arkham.Game qualified as Game
import Arkham.Game.Settings
import Arkham.Git
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Types
import Arkham.LocationSymbol
import Arkham.Matcher hiding (DuringTurn, FastPlayerWindow)
import Arkham.Name
import Arkham.Phase
import Arkham.Projection
import Arkham.Scenario.Scenarios.TheGathering (TheGathering (..))
import Arkham.Scenario.Types
import Arkham.SkillTest.Type
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait (Trait (Elite))
import Control.Monad.State
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.These
import GHC.OverloadedLabels
import GHC.TypeLits
import System.Random (StdGen, mkStdGen)

runMessages :: TestAppT ()
runMessages = do
  logger <- gets testLogger
  env <- get
  runReaderT (Game.runMessages logger) env

pushAndRun :: Message -> TestAppT ()
pushAndRun msg = push msg >> runMessages

run :: Message -> TestAppT ()
run = pushAndRun

pushAndRunAll :: [Message] -> TestAppT ()
pushAndRunAll msgs = pushAll msgs >> runMessages

runAll :: [Message] -> TestAppT ()
runAll = pushAndRunAll

shouldContainM
  :: (HasCallStack, Show a, MonadIO m, Eq a) => m [a] -> [a] -> m ()
x `shouldContainM` y = liftIO . (`shouldContain` y) =<< x

shouldNotContainM
  :: (HasCallStack, Show a, MonadIO m, Eq a) => m [a] -> [a] -> m ()
x `shouldNotContainM` y = liftIO . (`shouldNotContain` y) =<< x

shouldSatisfyM
  :: (HasCallStack, Show a, MonadIO m) => m a -> (a -> Bool) -> m ()
x `shouldSatisfyM` y = liftIO . (`shouldSatisfy` y) =<< x

shouldMatchListM
  :: (HasCallStack, Show a, Eq a, MonadIO m) => m [a] -> [a] -> m ()
x `shouldMatchListM` y = liftIO . (`shouldMatchList` y) =<< x

refShouldBe :: (HasCallStack, Show a, Eq a, MonadIO m) => IORef a -> a -> m ()
ref `refShouldBe` y = do
  result <- liftIO $ atomicModifyIORef ref (\x -> (x, x))
  liftIO $ result `shouldBe` y

nonFast :: Window
nonFast = Window Timing.When NonFast Nothing

fastPlayerWindow :: Window
fastPlayerWindow = Window Timing.When FastPlayerWindow Nothing

duringTurn :: InvestigatorId -> Window
duringTurn iid = Window Timing.When (DuringTurn iid) Nothing

data TestApp = TestApp
  { game :: IORef Game
  , messageQueueRef :: Queue Message
  , gen :: IORef StdGen
  , testLogger :: Maybe (Message -> IO ())
  , testGameLogger :: ClientMessage -> IO ()
  , debugLevel :: IORef Int
  }

cloneTestApp :: TestApp -> IO TestApp
cloneTestApp testApp = do
  game <- newIORef =<< readIORef (game testApp)
  messageQueueRef <- fmap Queue . newIORef =<< readIORef (queueToRef $ messageQueueRef testApp)
  gen <- newIORef =<< readIORef (gen testApp)
  pure
    $ TestApp
      { game = game
      , messageQueueRef = messageQueueRef
      , gen = gen
      , testLogger = testLogger testApp
      , testGameLogger = testGameLogger testApp
      , debugLevel = debugLevel testApp
      }

newtype TestAppT a = TestAppT {unTestAppT :: StateT TestApp IO a}
  deriving newtype (MonadState TestApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom)

instance HasDebugLevel TestAppT where
  getDebugLevel = liftIO . readIORef =<< gets debugLevel

instance HasGame TestAppT where
  getGame = do
    env <- get
    atomicModifyIORef (game env) (\x -> (x, x))

instance CardGen TestAppT where
  genEncounterCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let card = lookupEncounterCard (toCardDef a) cardId
    ref <- gets game
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = insertMap cardId (EncounterCard card) (gameCards g)}, ())
    pure card
  genPlayerCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let card = lookupPlayerCard (toCardDef a) cardId
    ref <- gets game
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = insertMap cardId (PlayerCard card) (gameCards g)}, ())
    pure card
  replaceCard cardId card = do
    ref <- gets game
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = insertMap cardId card (gameCards g)}, ())

runTestApp :: TestApp -> TestAppT a -> IO a
runTestApp testApp = flip evalStateT testApp . unTestAppT

instance HasGameRef TestApp where
  gameRefL = lens game $ \m x -> m {game = x}

instance HasStdGen TestApp where
  genL = lens gen $ \m x -> m {gen = x}

instance HasQueue Message TestAppT where
  messageQueue = gets messageQueueRef

instance HasQueue Message (ReaderT TestApp TestAppT) where
  messageQueue = asks messageQueueRef

instance HasGameLogger TestAppT where
  getLogger = do
    logger <- gets testGameLogger
    pure $ \msg -> liftIO $ logger msg

testScenario
  :: MonadIO m
  => CardCode
  -> (ScenarioAttrs -> ScenarioAttrs)
  -> m Scenario
testScenario cardCode f = do
  let name = mkName $ unCardCode cardCode
  pure
    . Scenario
    $ scenario (TheGathering . f) cardCode name Easy []

buildEvent :: CardGen m => CardDef -> Investigator -> m Event
buildEvent cardDef investigator = do
  card <- genCard cardDef
  createEvent card (toId investigator) <$> getRandom

buildEnemy :: HasCallStack => CardGen m => CardCode -> m Enemy
buildEnemy cardCode = case lookupCardDef cardCode of
  Nothing -> error $ "Test used invalid card code" <> show cardCode
  Just def -> do
    card <- genCard def
    lookupEnemy cardCode <$> getRandom <*> pure (toCardId card)

buildAsset
  :: CardGen m => CardDef -> Maybe Investigator -> m Asset
buildAsset cardDef mOwner = do
  card <- genCard cardDef
  lookupAsset (toCardCode card)
    <$> getRandom
    <*> pure (toId <$> mOwner)
    <*> pure (toCardId card)

testPlayerCards :: CardGen m => Int -> m [PlayerCard]
testPlayerCards count' = replicateM count' (testPlayerCard id)

testPlayerCard :: CardGen m => (CardDef -> CardDef) -> m PlayerCard
testPlayerCard f = genPlayerCard (f Cards.leatherCoat)

class TestUpdate a where
  updateLens :: a -> Traversal' Game a

instance TestUpdate Investigator where
  updateLens a = entitiesL . Entities.investigatorsL . ix a.id

instance TestUpdate Enemy where
  updateLens a = entitiesL . Entities.enemiesL . ix a.id

instance TestUpdate Location where
  updateLens a = entitiesL . Entities.locationsL . ix a.id

updateThis
  :: forall a. (TestUpdate a, Entity a) => a -> (EntityAttrs a -> EntityAttrs a) -> TestAppT a
updateThis this f = do
  let this' = overAttrs f this
  overTest $ updateLens this .~ this'
  pure this'

class TestHasFight a where
  setFight :: Int -> TestAppT a -> TestAppT a

instance TestHasFight Enemy where
  setFight fight action = do
    this <- action
    updateThis this $ \attrs -> attrs {enemyFight = Just fight}

class TestHasHealth a where
  setHealth :: Int -> TestAppT a -> TestAppT a

instance TestHasHealth Enemy where
  setHealth health action = do
    this <- action
    updateThis this $ \attrs -> attrs {enemyHealth = Just (Static health)}

class UpdateField (s :: Symbol) a b | s a -> b where
  updateField :: b -> a -> TestAppT a

prop :: forall (s :: Symbol) b a. (TestUpdate a, UpdateField s a b) => b -> TestAppT a -> TestAppT a
prop b action = do
  this' <- updateField @s b =<< action
  overTest $ updateLens this' .~ this'
  pure this'

updateProp :: forall s a b. (TestUpdate a, UpdateField s a b) => b -> a -> TestAppT ()
updateProp b a = void $ prop @s b (pure a)

withProp
  :: forall (s :: Symbol) b
   . UpdateField s Investigator b
  => b
  -> Investigator
  -> TestAppT ()
withProp b a = void $ prop @s b (getInvestigator $ toId a)

withPropM
  :: forall (s :: Symbol) b
   . UpdateField s Investigator b
  => TestAppT b
  -> Investigator
  -> TestAppT ()
withPropM action a = do
  b <- action
  void $ prop @s b (getInvestigator $ toId a)

withProps :: Investigator -> (TestAppT Investigator -> TestAppT Investigator) -> TestAppT ()
withProps self props = void $ pure self & props

instance IsLabel "willpower" (Int -> Investigator -> TestAppT Investigator) where
  fromLabel n s = pure s & prop @"willpower" n

instance UpdateField "remainingActions" Investigator Int where
  updateField remaining = pure . overAttrs (\attrs -> attrs {investigatorRemainingActions = remaining})

instance UpdateField "combat" Investigator Int where
  updateField combat = pure . overAttrs (\attrs -> attrs {investigatorCombat = combat})

instance UpdateField "willpower" Investigator Int where
  updateField willpower = pure . overAttrs (\attrs -> attrs {investigatorWillpower = willpower})

instance UpdateField "intellect" Investigator Int where
  updateField intellect = pure . overAttrs (\attrs -> attrs {investigatorIntellect = intellect})

instance UpdateField "agility" Investigator Int where
  updateField agility = pure . overAttrs (\attrs -> attrs {investigatorAgility = agility})

instance UpdateField "health" Investigator Int where
  updateField health = pure . overAttrs (\attrs -> attrs {investigatorHealth = health})

instance UpdateField "sanity" Investigator Int where
  updateField sanity = pure . overAttrs (\attrs -> attrs {investigatorSanity = sanity})

instance UpdateField "deck" Investigator (Deck PlayerCard) where
  updateField cards = pure . overAttrs (\attrs -> attrs {investigatorDeck = cards})

instance UpdateField "hand" Investigator [Card] where
  updateField cards = pure . overAttrs (\attrs -> attrs {investigatorHand = cards})

instance UpdateField "bonded" Investigator [Card] where
  updateField cards = pure . overAttrs (\attrs -> attrs {investigatorBondedCards = cards})

instance UpdateField "discard" Investigator [PlayerCard] where
  updateField cards i = pure $ overAttrs (\attrs -> attrs {investigatorDiscard = cards}) i

instance UpdateField "resources" Investigator Int where
  updateField resources =
    pure . overAttrs (Arkham.Investigator.Types.tokensL %~ setTokens Resource resources)

instance UpdateField "fight" Enemy Int where
  updateField fight = pure . overAttrs (\attrs -> attrs {enemyFight = Just fight})

instance UpdateField "evade" Enemy Int where
  updateField evade = pure . overAttrs (\attrs -> attrs {enemyEvade = Just evade})

instance UpdateField "health" Enemy Int where
  updateField health = pure . overAttrs (\attrs -> attrs {enemyHealth = Just (Static health)})

instance UpdateField "healthDamage" Enemy Int where
  updateField damage = pure . overAttrs (\attrs -> attrs {enemyHealthDamage = damage})

instance UpdateField "sanityDamage" Enemy Int where
  updateField damage = pure . overAttrs (\attrs -> attrs {enemySanityDamage = damage})

instance UpdateField "exhausted" Enemy Bool where
  updateField isExhausted = pure . overAttrs (\attrs -> attrs {enemyExhausted = isExhausted})

instance UpdateField "elite" Enemy Bool where
  updateField True this = do
    run $ gameModifier (TestSource mempty) (toTarget this) (AddTrait Elite)
    pure this
  updateField False _ = error "Cannot set elite to false"

instance UpdateField "hunter" Enemy Bool where
  updateField True this = do
    run $ gameModifier (TestSource mempty) (toTarget this) (AddKeyword Keyword.Hunter)
    pure this
  updateField False _ = error "Cannot set hunter to false"

exhausted :: forall a. (TestUpdate a, UpdateField "exhausted" a Bool) => TestAppT a -> TestAppT a
exhausted = prop @"exhausted" True

elite :: forall a. (TestUpdate a, UpdateField "elite" a Bool) => TestAppT a -> TestAppT a
elite = prop @"elite" True

hunter :: forall a. (TestUpdate a, UpdateField "hunter" a Bool) => TestAppT a -> TestAppT a
hunter = prop @"hunter" True

instance UpdateField "clues" Location Int where
  updateField clues =
    pure
      . overAttrs
        ( \attrs ->
            attrs
              { locationTokens = setTokens Clue clues mempty
              , locationRevealClues = Static 0
              , locationWithoutClues = clues == 0
              }
        )

instance UpdateField "revealed" Location Bool where
  updateField revealed' = pure . overAttrs (\attrs -> attrs {locationRevealed = revealed'})

instance UpdateField "shroud" Location Int where
  updateField shroud = pure . overAttrs (\attrs -> attrs {locationShroud = shroud})

instance UpdateField "damage" Investigator Int where
  updateField damage = pure . overAttrs (Arkham.Investigator.Types.tokensL %~ setTokens #damage damage)

instance UpdateField "horror" Investigator Int where
  updateField horror = pure . overAttrs (Arkham.Investigator.Types.tokensL %~ setTokens #horror horror)

testEnemy :: TestAppT Enemy
testEnemy = testEnemyWithDef Cards.swarmOfRats id

testEnemyWith :: (EnemyAttrs -> EnemyAttrs) -> TestAppT Enemy
testEnemyWith = testEnemyWithDef Cards.swarmOfRats

testEnemyWithDef
  :: CardDef
  -> (EnemyAttrs -> EnemyAttrs)
  -> TestAppT Enemy
testEnemyWithDef def attrsF = do
  card <- genCard def
  enemyId <- getRandom
  let enemy' =
        overAttrs (\attrs -> attrsF $ attrs {enemyHealthDamage = 0, enemySanityDamage = 0})
          $ lookupEnemy (toCardCode card) enemyId (toCardId card)
  overTest $ entitiesL . Entities.enemiesL %~ insertEntity enemy'
  pure enemy'

overTest :: (Game -> Game) -> TestAppT ()
overTest body = get >>= runReaderT (overGame body)

testAsset
  :: (AssetAttrs -> AssetAttrs)
  -> Investigator
  -> TestAppT Asset
testAsset f i = testAssetWithDef Cards.adaptable1 f i

testAssetWithDef
  :: CardDef
  -> (AssetAttrs -> AssetAttrs)
  -> Investigator
  -> TestAppT Asset
testAssetWithDef def attrsF owner = do
  card <- genCard def
  assetId <- getRandom
  let
    asset' =
      overAttrs attrsF
        $ lookupAsset (toCardCode card) assetId (Just $ toId owner) (toCardId card)
  env <- get
  runReaderT (overGame (entitiesL . Entities.assetsL %~ insertEntity asset')) env
  pure asset'

testAgenda
  :: CardCode
  -> (AgendaAttrs -> AgendaAttrs)
  -> TestAppT Agenda
testAgenda cardCode f = do
  card <- genCard Cards.whatsGoingOn
  let
    agenda' =
      cbCardBuilder
        ( Agenda <$> agendaWith (1, A) WhatsGoingOn Cards.whatsGoingOn (Static 100) f
        )
        (toCardId card)
        (1, AgendaId cardCode)
  env <- get
  runReaderT (overGame (entitiesL . Entities.agendasL %~ insertEntity agenda')) env
  pure agenda'

testLocation :: TestAppT Location
testLocation = testLocationWithDef Cards.study id

testLocationWith :: (LocationAttrs -> LocationAttrs) -> TestAppT Location
testLocationWith = testLocationWithDef Cards.study

testLocationWithDef
  :: CardDef
  -> (LocationAttrs -> LocationAttrs)
  -> TestAppT Location
testLocationWithDef def attrsF = do
  card <- genCard def
  locationId <- getRandom
  let location' = overAttrs attrsF $ lookupLocation (toCardCode card) locationId (toCardId card)
  env <- get
  runReaderT (overGame (entitiesL . Entities.locationsL %~ insertEntity location')) env
  pure location'

testInvestigator
  :: MonadIO m
  => CardDef
  -> m Investigator
testInvestigator cardDef = do
  playerId <- liftIO getRandom
  pure $ lookupInvestigator (InvestigatorId $ toCardCode cardDef) playerId

{- | We use Jenny Barnes because here abilities are the least
disruptive during tests since they won't add extra windows
or abilities
-}
testJenny :: MonadIO m => m Investigator
testJenny = testInvestigator Investigators.jennyBarnes

addInvestigator
  :: CardDef
  -> TestAppT Investigator
addInvestigator defF = do
  investigator' <- testInvestigator defF
  env <- get
  runReaderT (overGame (entitiesL . Entities.investigatorsL %~ insertEntity investigator')) env
  pure investigator'

testConnectedLocations
  :: (LocationAttrs -> LocationAttrs)
  -> (LocationAttrs -> LocationAttrs)
  -> TestAppT (Location, Location)
testConnectedLocations f1 f2 = testConnectedLocationsWithDef (Cards.rivertown, f1) (Cards.southsideHistoricalSociety, f2)

testConnectedLocationsWithDef
  :: (CardDef, LocationAttrs -> LocationAttrs)
  -> (CardDef, LocationAttrs -> LocationAttrs)
  -> TestAppT (Location, Location)
testConnectedLocationsWithDef (def1, attrsF1) (def2, attrsF2) = do
  location1 <-
    testLocationWithDef
      def1
      ( attrsF1
          . (symbolL .~ Square)
          . (revealedSymbolL .~ Square)
          . (connectedMatchersL .~ [LocationWithSymbol Triangle])
          . (revealedConnectedMatchersL .~ [LocationWithSymbol Triangle])
      )
  location2 <-
    testLocationWithDef
      def2
      ( attrsF2
          . (symbolL .~ Triangle)
          . (revealedSymbolL .~ Triangle)
          . (connectedMatchersL .~ [LocationWithSymbol Square])
          . (revealedConnectedMatchersL .~ [LocationWithSymbol Square])
      )
  pure (location1, location2)

testUnconnectedLocations
  :: (LocationAttrs -> LocationAttrs)
  -> (LocationAttrs -> LocationAttrs)
  -> TestAppT (Location, Location)
testUnconnectedLocations f1 f2 = do
  location1 <-
    testLocationWith
      (f1 . (symbolL .~ Square) . (revealedSymbolL .~ Square))
  location2 <-
    testLocationWith
      (f2 . (symbolL .~ Triangle) . (revealedSymbolL .~ Triangle))
  pure (location1, location2)

createMessageMatcher :: Message -> TestAppT (IORef Bool)
createMessageMatcher msg = createMessageChecker (== msg)

createMessageChecker :: (Message -> Bool) -> TestAppT (IORef Bool)
createMessageChecker f = do
  ref <- liftIO $ newIORef False
  testApp <- get
  put
    $ testApp
      { testLogger =
          Just (\msg -> when (f msg) (liftIO $ atomicWriteIORef ref True))
      }
  pure ref

didPassSkillTestBy
  :: Investigator
  -> SkillType
  -> Int
  -> TestAppT (IORef Bool)
didPassSkillTestBy investigator skillType n =
  createMessageMatcher
    ( PassedSkillTest
        (toId investigator)
        Nothing
        (TestSource mempty)
        (SkillTestInitiatorTarget TestTarget)
        (SkillSkillTest skillType)
        n
    )

didFailSkillTestBy
  :: Investigator
  -> SkillType
  -> Int
  -> TestAppT (IORef Bool)
didFailSkillTestBy investigator skillType n =
  createMessageMatcher
    ( FailedSkillTest
        (toId investigator)
        Nothing
        (TestSource mempty)
        (SkillTestInitiatorTarget TestTarget)
        (SkillSkillTest skillType)
        n
    )

assert :: HasCallStack => TestAppT Bool -> TestAppT ()
assert body = do
  result <- body
  liftIO $ result `shouldBe` True

assertAny :: (HasCallStack, Query a) => a -> TestAppT ()
assertAny = assert . selectAny

assertNone :: (HasCallStack, Query a) => a -> TestAppT ()
assertNone = assert . selectNone

withGame :: (MonadReader env m, HasGame m) => ReaderT Game m b -> m b
withGame b = do
  g <- getGame
  runReaderT b g

replaceScenario
  :: (MonadReader env m, HasGameRef env, MonadIO m)
  => (ScenarioAttrs -> ScenarioAttrs)
  -> m ()
replaceScenario f = do
  scenario' <- testScenario "00000" f
  ref <- view gameRefL
  atomicModifyIORef' ref (\g -> (g {gameMode = That scenario'}, ()))

chooseOnlyOption :: HasCallStack => String -> TestAppT ()
chooseOnlyOption _reason = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne [msg] -> push (uiToRun msg) <* runMessages
      ChooseOneAtATime [msg] -> push (uiToRun msg) <* runMessages
      ChooseN _ [msg] -> push (uiToRun msg) <* runMessages
      Read {} -> runMessages
      _ -> error "spec expectation mismatch"
    _ -> error "There must be only one choice to use this function"

chooseFirstOption :: HasCallStack => String -> TestAppT ()
chooseFirstOption _reason = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne (msg : _) -> push (uiToRun msg) >> runMessages
      ChooseOneAtATime (msg : _) -> push (uiToRun msg) >> runMessages
      _ -> error "spec expectation mismatch"
    _ -> error "There must be at least one option"

chooseOptionMatching :: HasCallStack => String -> (UI Message -> Bool) -> TestAppT ()
chooseOptionMatching _reason f = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(iid, question)] -> go iid question
    _ -> error "There must be only one question to use this function"
 where
  go iid question = case question of
    QuestionLabel _ _ q -> go iid q
    ChooseOne msgs -> case find f msgs of
      Just msg -> push (uiToRun msg) <* runMessages
      Nothing -> liftIO $ expectationFailure "could not find a matching message"
    ChooseOneAtATime msgs -> case find f msgs of
      Just msg -> do
        pushIfAny (deleteFirst msg msgs) (Ask iid $ ChooseOneAtATime $ deleteFirst msg msgs)
        push (uiToRun msg)
        runMessages
      Nothing -> liftIO $ expectationFailure "could not find a matching message"
    ChooseN n msgs -> case find f msgs of
      Just msg -> do
        pushWhen (n > 1) (Ask iid $ ChooseN (n - 1) $ deleteFirst msg msgs)
        push (uiToRun msg)
        runMessages
      Nothing -> liftIO $ expectationFailure "could not find a matching message"
    _ -> error $ "unsupported questions type: " <> show question

debug :: (Investigator -> TestAppT ()) -> Investigator -> TestAppT ()
debug f i = do
  ref <- gets debugLevel
  atomicWriteIORef ref 1
  f i

debugV :: (Investigator -> TestAppT ()) -> Investigator -> TestAppT ()
debugV f i = do
  ref <- gets debugLevel
  atomicWriteIORef ref 2
  f i

{- | Run a test with a default investigator.
We use jenny barnes because she has no direct interaction with the game state
-}
gameTest :: (Investigator -> TestAppT ()) -> IO ()
gameTest = gameTestWith Investigators.jennyBarnes

gameTestWith :: CardDef -> (Investigator -> TestAppT ()) -> IO ()
gameTestWith investigatorDef body = do
  investigator <- testInvestigator investigatorDef
  g <- newGame investigator
  gameRef <- newIORef g
  queueRef <- newQueue []
  genRef <- newIORef $ mkStdGen (gameSeed g)
  debugLevelRef <- newIORef 0
  let testApp = TestApp gameRef queueRef genRef Nothing (pure . const ()) debugLevelRef
  runReaderT (overGameM preloadModifiers) testApp
  runTestApp testApp (body investigator)

newGame :: MonadIO m => Investigator -> m Game
newGame investigator = do
  scenario' <- testScenario "01104" id
  seed <- liftIO getRandom
  let
    game =
      Game
        { gameWindowDepth = 0
        , gameRunWindows = True
        , gameDepthLock = 0
        , gamePhaseHistory = mempty
        , gameRoundHistory = mempty
        , gameTurnHistory = mempty
        , gameTurnPlayerInvestigatorId = Just investigatorId
        , gameSeed = seed
        , gameSettings = defaultSettings
        , gameInitialSeed = seed
        , gameMode = That scenario'
        , gamePlayerCount = 1
        , gameActiveInvestigatorId = investigatorId
        , gameLeadInvestigatorId = investigatorId
        , gameActivePlayerId = attr investigatorPlayerId investigator
        , gamePlayers = [attr investigatorPlayerId investigator]
        , gamePhase = CampaignPhase -- TODO: maybe this should be a TestPhase or something?
        , gamePhaseStep = Nothing
        , gameSkillTest = Nothing
        , gameSkillTestResults = Nothing
        , gameEntities =
            defaultEntities
              { entitiesInvestigators = Map.singleton investigatorId investigator
              }
        , gameModifiers = mempty
        , gameEncounterDiscardEntities = defaultEntities
        , gameInHandEntities = mempty
        , gameInDiscardEntities = mempty
        , gameOutOfPlayEntities = mempty
        , gameActionRemovedEntities = mempty
        , gameInSearchEntities = defaultEntities
        , gameGameState = IsActive
        , gameFoundCards = mempty
        , gameFocusedCards = mempty
        , gameFocusedTarotCards = mempty
        , gameFocusedChaosTokens = mempty
        , gameActiveCard = Nothing
        , gameResolvingCard = Nothing
        , gamePlayerOrder = [investigatorId]
        , gameRemovedFromPlay = mempty
        , gameEnemyMoving = Nothing
        , gameQuestion = mempty
        , gameActionCanBeUndone = False
        , gameActionDiff = []
        , gameInAction = False
        , gameCards = mempty
        , gameActiveCost = mempty
        , gameActiveAbilities = mempty
        , gameInSetup = False
        , gameIgnoreCanModifiers = False
        , gameEnemyEvading = Nothing
        , gameGitRevision = gitHash
        , gameCardUses = mempty
        , gameAllowEmptySpaces = False
        , gamePerformTarotReadings = False
        , gameCurrentBatchId = Nothing
        }

  liftIO $ do
    gameRef <- newIORef game
    queueRef <- Queue <$> newIORef []
    genRef <- newIORef $ mkStdGen (gameSeed game)
    debugLevelRef <- newIORef 0

    runTestApp (TestApp gameRef queueRef genRef Nothing (pure . const ()) debugLevelRef) $ do
      a1 <- testAgenda "01105" id
      let s'' = overAttrs (agendaStackL .~ IntMap.fromList [(1, [toCard a1, toCard a1])]) scenario'
      pure $ game {gameMode = That s''}
 where
  investigatorId = toId investigator

-- Helpers

isInDiscardOf
  :: HasCardDef cardDef => cardDef -> Investigator -> TestAppT Bool
isInDiscardOf (toCardDef -> cardDef) i = do
  fieldP InvestigatorDiscard (any (`cardMatch` cardIs cardDef)) (toId i)

getRemainingActions :: Investigator -> TestAppT Int
getRemainingActions = field InvestigatorRemainingActions . toId

getActiveCost :: TestAppT ActiveCost
getActiveCost =
  snd
    . fromJustNote "no active cost for test"
    . headMay
    . mapToList
    . gameActiveCost
    <$> getGame

evadedBy :: Investigator -> Enemy -> TestAppT Bool
evadedBy _investigator = fieldP EnemyEngagedInvestigators null . toId

class ConvertToEntityId a b | a -> b where
  toEntityId :: a -> b

instance ConvertToEntityId Agenda AgendaId where
  toEntityId = toId

instance ConvertToEntityId Enemy EnemyId where
  toEntityId = toId

instance ConvertToEntityId Asset AssetId where
  toEntityId = toId

instance ConvertToEntityId AssetId AssetId where
  toEntityId = id

instance ConvertToEntityId Location LocationId where
  toEntityId = toId

instance ConvertToEntityId Investigator InvestigatorId where
  toEntityId = toId

fieldAssert
  :: (HasCallStack, Projection attrs, ConvertToEntityId a (EntityId attrs))
  => Field attrs typ
  -> (typ -> Bool)
  -> a
  -> TestAppT ()
fieldAssert fld p a = do
  result <- fieldP fld p (toEntityId a)
  liftIO $ result `shouldBe` True

fieldAssertLength
  :: (HasCallStack, Projection attrs, ConvertToEntityId a (EntityId attrs))
  => Field attrs [typ]
  -> Int
  -> a
  -> TestAppT ()
fieldAssertLength fld n = fieldAssert fld ((== n) . length)

handIs :: [Card] -> Investigator -> TestAppT Bool
handIs cards = fieldP InvestigatorHand (== cards) . toId

putCardIntoPlay :: HasCardDef def => Investigator -> def -> TestAppT ()
putCardIntoPlay i (toCardDef -> def) = do
  card <- genCard def
  let
    card' = case card of
      PlayerCard pc -> PlayerCard $ pc {pcOwner = Just $ toId i}
      other -> other
  pushAndRun $ PutCardIntoPlay (toId i) card' Nothing []

playEvent :: HasCardDef def => Investigator -> def -> TestAppT ()
playEvent = putCardIntoPlay

putAssetIntoPlay :: HasCardDef def => Investigator -> def -> TestAppT AssetId
putAssetIntoPlay i (toCardDef -> def) = do
  card <- genCard def
  let
    card' = case card of
      PlayerCard pc -> PlayerCard $ pc {pcOwner = Just $ toId i}
      other -> other
  pushAndRun $ PutCardIntoPlay (toId i) card' Nothing []
  selectJust $ AssetWithCardId (toCardId card)

putTreacheryIntoPlay :: HasCardDef def => Investigator -> def -> TestAppT TreacheryId
putTreacheryIntoPlay i (toCardDef -> def) = do
  card <- genCard def
  let
    card' = case card of
      PlayerCard pc -> PlayerCard $ pc {pcOwner = Just $ toId i}
      other -> other
  pushAndRun $ PutCardIntoPlay (toId i) card' Nothing []
  selectJust $ TreacheryWithCardId (toCardId card)

updateInvestigator :: Investigator -> (InvestigatorAttrs -> InvestigatorAttrs) -> TestAppT ()
updateInvestigator i f = do
  env <- get
  runReaderT
    (overGame (entitiesL . Entities.investigatorsL . ix (toId i) %~ overAttrs f))
    env

duringRound :: TestAppT () -> TestAppT ()
duringRound body = do
  run BeginRound
  body
  run EndRound

assignDamageTo :: Sourceable source => Investigator -> source -> TestAppT ()
assignDamageTo _ (toSource -> source) = case source of
  AssetSource aid -> chooseOptionMatching "assign to asset" $ \case
    ComponentLabel component _ -> case component of
      AssetComponent aid' _ -> aid == aid'
      _ -> False
    _ -> False
  _ -> error "unhandled source, consider adding it to assignDamageTo"

withDeck :: HasCardDef a => Investigator -> [a] -> TestAppT ()
withDeck self cs = do
  deck <- Deck <$> traverse genPlayerCard cs
  withProp @"deck" deck self

withHand :: HasCardDef a => Investigator -> [a] -> TestAppT ()
withHand self cs = do
  hand <- traverse genCard cs
  withProp @"hand" hand self

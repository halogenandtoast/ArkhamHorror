{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestImport (
  module X,
  module TestImport,
) where

import Arkham.Prelude as X hiding (assert)

import Arkham.ActiveCost
import Arkham.Agenda as X
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Cards.WhatsGoingOn
import Arkham.Agenda.Types
import Arkham.Asset as X (createAsset, lookupAsset)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types
import Arkham.Card as X
import Arkham.Card.EncounterCard as X
import Arkham.Card.PlayerCard as X
import Arkham.ChaosBag as X
import Arkham.Classes as X hiding (getTokenValue)
import Arkham.Cost as X hiding (PaidCost)
import Arkham.Difficulty
import Arkham.Enemy as X
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types
import Arkham.Event as X
import Arkham.Event.Types
import Arkham.Game as X hiding (addInvestigator, getAsset, newGame, runMessages)
import Arkham.Game qualified as Game
import Arkham.Game.Helpers as X hiding (getCanAffordCost)
import Arkham.GameValue as X
import Arkham.Helpers as X
import Arkham.Helpers.Message as X hiding (createEnemy)
import Arkham.Id as X
import Arkham.Investigator as X
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types hiding (assetsL)
import Arkham.Location as X
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Types
import Arkham.Matcher hiding (DuringTurn, FastPlayerWindow)
import Arkham.Message as X
import Arkham.Phase
import Arkham.Projection
import Arkham.Scenario as X
import Arkham.Scenario.Scenarios.TheGathering (TheGathering (..))
import Arkham.Scenario.Types
import Arkham.SkillType as X
import Arkham.Source as X
import Arkham.Stats as X
import Arkham.Target as X
import Arkham.Timing qualified as Timing
import Arkham.Token as X hiding (TokenId)
import Arkham.Window as X (
  Window (..),
  WindowType (DuringTurn, FastPlayerWindow, NonFast),
 )
import Control.Lens as X (set, (^?!))
import Control.Monad.Fail as X
import Control.Monad.State
import Control.Monad.State as X (get)
import Data.Map.Strict qualified as Map
import Data.Maybe as X (fromJust)
import Data.These
import Data.UUID.V4 as X
import Helpers.Message as X
import System.Random (StdGen, mkStdGen)
import Test.Hspec as X

import Arkham.Agenda.Sequence
import Arkham.Entities as X
import Arkham.Entities qualified as Entities
import Arkham.GameEnv
import Arkham.LocationSymbol
import Arkham.Name
import Arkham.SkillTest.Type
import Data.IntMap.Strict qualified as IntMap

runMessages :: TestAppT ()
runMessages = do
  logger <- gets testLogger
  env <- get
  runReaderT (Game.runMessages logger) env

pushAndRun :: Message -> TestAppT ()
pushAndRun msg = push msg >> runMessages

pushAndRunAll :: [Message] -> TestAppT ()
pushAndRunAll msgs = pushAll msgs >> runMessages

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
nonFast = Window Timing.When NonFast

fastPlayerWindow :: Window
fastPlayerWindow = Window Timing.When FastPlayerWindow

duringTurn :: InvestigatorId -> Window
duringTurn = Window Timing.When . DuringTurn

data TestApp = TestApp
  { game :: IORef Game
  , messageQueueRef :: Queue Message
  , gen :: IORef StdGen
  , testLogger :: Maybe (Message -> IO ())
  , testGameLogger :: Text -> IO ()
  }

newtype TestAppT a = TestAppT {unTestAppT :: StateT TestApp IO a}
  deriving newtype (MonadState TestApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom)

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

instance HasGameLogger TestApp where
  gameLoggerL = lens testGameLogger $ \m x -> m {testGameLogger = x}

testScenario
  :: (MonadIO m)
  => CardCode
  -> (ScenarioAttrs -> ScenarioAttrs)
  -> m Scenario
testScenario cardCode f = do
  let name = mkName $ unCardCode cardCode
  pure
    . Scenario
    $ scenario (TheGathering . f) cardCode name Easy []

buildEvent :: (CardGen m) => CardDef -> Investigator -> m Event
buildEvent cardDef investigator = do
  card <- genCard cardDef
  createEvent card (toId investigator) <$> getRandom

buildEnemy :: (HasCallStack) => (CardGen m) => CardCode -> m Enemy
buildEnemy cardCode = case lookupCardDef cardCode of
  Nothing -> error $ "Test used invalid card code" <> show cardCode
  Just def -> do
    card <- genCard def
    lookupEnemy cardCode <$> getRandom <*> pure (toCardId card)

buildAsset
  :: (CardGen m) => CardDef -> Maybe Investigator -> m Asset
buildAsset cardDef mOwner = do
  card <- genCard cardDef
  lookupAsset (toCardCode card)
    <$> getRandom
    <*> pure (toId <$> mOwner)
    <*> pure (toCardId card)

testPlayerCards :: (CardGen m) => Int -> m [PlayerCard]
testPlayerCards count' = replicateM count' (testPlayerCard id)

testPlayerCard :: (CardGen m) => (CardDef -> CardDef) -> m PlayerCard
testPlayerCard f = genPlayerCard (f Cards.adaptable1) -- use adaptable because it has no in game effects

testEnemy :: (EnemyAttrs -> EnemyAttrs) -> TestAppT Enemy
testEnemy = testEnemyWithDef Cards.swarmOfRats

testEnemyWithDef
  :: CardDef
  -> (EnemyAttrs -> EnemyAttrs)
  -> TestAppT Enemy
testEnemyWithDef def attrsF = do
  card <- genCard def
  enemyId <- getRandom
  let enemy' =
        overAttrs (\attrs -> attrsF $ attrs {enemyHealthDamage = 0, enemySanityDamage = 0}) $
          lookupEnemy (toCardCode card) enemyId (toCardId card)
  env <- get
  runReaderT (overGame (entitiesL . Entities.enemiesL %~ insertEntity enemy')) env
  pure enemy'

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
      overAttrs attrsF $
        lookupAsset (toCardCode card) assetId (Just $ toId owner) (toCardId card)
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

testLocation :: (LocationAttrs -> LocationAttrs) -> TestAppT Location
testLocation = testLocationWithDef Cards.study

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

{- | We use Jenny Barnes because here abilities are the least
disruptive during tests since they won't add extra windows
or abilities
-}
testInvestigator
  :: (MonadIO m)
  => CardDef
  -> (InvestigatorAttrs -> InvestigatorAttrs)
  -> m Investigator
testInvestigator cardDef f =
  pure $ overAttrs f $ lookupInvestigator (InvestigatorId $ toCardCode cardDef)

testJenny
  :: (MonadIO m) => (InvestigatorAttrs -> InvestigatorAttrs) -> m Investigator
testJenny = testInvestigator Investigators.jennyBarnes

addInvestigator
  :: CardDef
  -> (InvestigatorAttrs -> InvestigatorAttrs)
  -> TestAppT Investigator
addInvestigator defF attrsF = do
  investigator' <- testInvestigator defF attrsF
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
    testLocation
      (f1 . (symbolL .~ Square) . (revealedSymbolL .~ Square))
  location2 <-
    testLocation
      (f2 . (symbolL .~ Triangle) . (revealedSymbolL .~ Triangle))
  pure (location1, location2)

createMessageMatcher :: Message -> TestAppT (IORef Bool)
createMessageMatcher msg = createMessageChecker (== msg)

createMessageChecker :: (Message -> Bool) -> TestAppT (IORef Bool)
createMessageChecker f = do
  ref <- liftIO $ newIORef False
  testApp <- get
  put $
    testApp
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

assert :: TestAppT Bool -> TestAppT ()
assert body = do
  result <- body
  liftIO $ result `shouldBe` True

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

chooseOnlyOption :: (HasCallStack) => String -> TestAppT ()
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

chooseFirstOption :: (HasCallStack) => String -> TestAppT ()
chooseFirstOption _reason = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne (msg : _) -> push (uiToRun msg) >> runMessages
      ChooseOneAtATime (msg : _) -> push (uiToRun msg) >> runMessages
      _ -> error "spec expectation mismatch"
    _ -> error "There must be at least one option"

chooseOptionMatching :: (HasCallStack) => String -> (UI Message -> Bool) -> TestAppT ()
chooseOptionMatching _reason f = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne msgs -> case find f msgs of
        Just msg -> push (uiToRun msg) <* runMessages
        Nothing -> error "could not find a matching message"
      ChooseN _ msgs -> case find f msgs of
        Just msg -> push (uiToRun msg) <* runMessages
        Nothing -> error "could not find a matching message"
      _ -> error $ "unsupported questions type: " <> show question
    _ -> error "There must be only one question to use this function"

{- | Run a test with a default investigator.
We use jenny barnes because she has no direct interaction with the game state
-}
gameTest :: (Investigator -> TestAppT ()) -> IO ()
gameTest = gameTestWith Investigators.jennyBarnes

gameTestWith :: CardDef -> (Investigator -> TestAppT ()) -> IO ()
gameTestWith investigatorDef body = do
  investigator <- testInvestigator investigatorDef id
  g <- newGame investigator
  gameRef <- newIORef g
  queueRef <- newQueue []
  genRef <- newIORef $ mkStdGen (gameSeed g)
  let testApp = TestApp gameRef queueRef genRef Nothing (pure . const ())
  runReaderT (overGameM preloadModifiers) testApp
  runTestApp testApp (body investigator)

newGame :: (MonadIO m) => Investigator -> m Game
newGame investigator = do
  scenario' <- testScenario "01104" id
  seed <- liftIO getRandom
  let
    game =
      Game
        { gameWindowDepth = 0
        , gameDepthLock = 0
        , gamePhaseHistory = mempty
        , gameRoundHistory = mempty
        , gameTurnHistory = mempty
        , gameTurnPlayerInvestigatorId = Just investigatorId
        , gameSeed = seed
        , gameInitialSeed = seed
        , gameMode = That scenario'
        , gamePlayerCount = 1
        , gameActiveInvestigatorId = investigatorId
        , gameLeadInvestigatorId = investigatorId
        , gamePhase = CampaignPhase -- TODO: maybe this should be a TestPhase or something?
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
        , gameInSearchEntities = defaultEntities
        , gameGameState = IsActive
        , gameFoundCards = mempty
        , gameFocusedCards = mempty
        , gameFocusedTokens = mempty
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
        }

  liftIO $ do
    gameRef <- newIORef game
    queueRef <- Queue <$> newIORef []
    genRef <- newIORef $ mkStdGen (gameSeed game)

    runTestApp (TestApp gameRef queueRef genRef Nothing (pure . const ())) $ do
      a1 <- testAgenda "01105" id
      let s'' = overAttrs (agendaStackL .~ IntMap.fromList [(1, [toCard a1, toCard a1])]) scenario'
      pure $ game {gameMode = That s''}
 where
  investigatorId = toId investigator

-- Helpers

isInDiscardOf
  :: (HasCardDef cardDef) => Investigator -> cardDef -> TestAppT Bool
isInDiscardOf i (toCardDef -> cardDef) = do
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
  :: (HasCallStack, Projection attrs, ConvertToEntityId a b, b ~ EntityId attrs)
  => Field attrs typ
  -> (typ -> Bool)
  -> a
  -> TestAppT ()
fieldAssert fld p a = do
  result <- fieldP fld p (toEntityId a)
  liftIO $ result `shouldBe` True

handIs :: [Card] -> Investigator -> TestAppT Bool
handIs cards = fieldP InvestigatorHand (== cards) . toId

putCardIntoPlay :: (HasCardDef def) => Investigator -> def -> TestAppT ()
putCardIntoPlay i (toCardDef -> def) = do
  card <- genCard def
  let
    card' = case card of
      PlayerCard pc -> PlayerCard $ pc {pcOwner = Just $ toId i}
      other -> other
  pushAndRun $ PutCardIntoPlay (toId i) card' Nothing []

updateInvestigator :: Investigator -> (InvestigatorAttrs -> InvestigatorAttrs) -> TestAppT ()
updateInvestigator i f = do
  env <- get
  runReaderT
    (overGame (entitiesL . Entities.investigatorsL . ix (toId i) %~ overAttrs f))
    env

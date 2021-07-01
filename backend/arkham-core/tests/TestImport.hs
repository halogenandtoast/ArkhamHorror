{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestImport
  ( module X
  , module TestImport
  )
where

import Arkham.Prelude as X

import Arkham.EncounterCard
import Arkham.Game as X hiding (newGame)
import Arkham.PlayerCard
import Arkham.Types.Action
import Arkham.Types.Agenda as X
import Arkham.Types.Agenda.Attrs
import Arkham.Types.AgendaId
import Arkham.Types.Asset as X
import Arkham.Types.Asset.Attrs
import Arkham.Types.AssetId
import Arkham.Types.Card as X
import Arkham.Types.Card.Id
import Arkham.Types.ChaosBag as X
import qualified Arkham.Types.ChaosBag as ChaosBag
import Arkham.Types.Classes as X hiding
  (getCount, getId, getModifiersFor, getTokenValue)
import qualified Arkham.Types.Classes as Arkham
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost as X
import Arkham.Types.Difficulty
import Arkham.Types.Enemy as X
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Event as X
import Arkham.Types.Game as X hiding (getAsset)
import qualified Arkham.Types.Game as Game
import Arkham.Types.Game.Helpers as X hiding (getCanAffordCost)
import qualified Arkham.Types.Game.Helpers as Helpers
import Arkham.Types.GameValue as X
import Arkham.Types.Helpers as X
import Arkham.Types.Investigator as X
import Arkham.Types.Investigator.Attrs
import Arkham.Types.InvestigatorId
import Arkham.Types.Location as X
import Arkham.Types.Location.Attrs
import Arkham.Types.LocationId as X
import Arkham.Types.LocationSymbol
import Arkham.Types.Message as X
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Phase
import Arkham.Types.Query as X
import Arkham.Types.Scenario as X
import Arkham.Types.Scenario.Attrs
import Arkham.Types.SkillType as X
import Arkham.Types.Source as X
import Arkham.Types.Stats as X
import Arkham.Types.Target as X
import Arkham.Types.Token as X
import Arkham.Types.Window as X
import Control.Lens as X (set, (^?!))
import Control.Monad.Fail as X
import Control.Monad.State as X (get)
import Control.Monad.State hiding (replicateM)
import qualified Data.HashMap.Strict as HashMap
import Data.These
import qualified Data.UUID as UUID
import Data.UUID.V4 as X
import Helpers.Matchers as X
import Helpers.Message as X
import System.Random (StdGen, mkStdGen)
import Test.Hspec as X

shouldSatisfyM
  :: (HasCallStack, Show a, MonadIO m) => m a -> (a -> Bool) -> m ()
x `shouldSatisfyM` y = liftIO . (`shouldSatisfy` y) =<< x

shouldMatchListM
  :: (HasCallStack, Show a, Eq a, MonadIO m) => m [a] -> [a] -> m ()
x `shouldMatchListM` y = liftIO . (`shouldMatchList` y) =<< x

refShouldBe :: (HasCallStack, Show a, Eq a, MonadIO m) => IORef a -> a -> m ()
ref `refShouldBe` y = do
  result <- liftIO $ readIORef ref
  liftIO $ result `shouldBe` y

getId
  :: ( HasId id GameEnv a
     , HasGameRef env
     , HasQueue env
     , HasStdGen env
     , MonadReader env m
     , MonadIO m
     )
  => a
  -> m id
getId a = toGameEnv >>= runReaderT (Arkham.getId a)

getCount
  :: ( HasCount count GameEnv a
     , HasGameRef env
     , HasQueue env
     , HasStdGen env
     , MonadReader env m
     , MonadIO m
     )
  => a
  -> m count
getCount a = toGameEnv >>= runReaderT (Arkham.getCount a)

getAsset
  :: ( HasCallStack
     , MonadReader env m
     , HasGameRef env
     , MonadIO m
     , HasQueue env
     , HasStdGen env
     )
  => AssetId
  -> m Asset
getAsset aid = toGameEnv >>= runReaderT (Game.getAsset aid)

getTokenValue
  :: ( MonadReader env m
     , MonadIO m
     , HasGameRef env
     , HasQueue env
     , HasStdGen env
     , HasTokenValue GameEnv a
     )
  => a
  -> InvestigatorId
  -> Token
  -> m TokenValue
getTokenValue a iid token =
  toGameEnv >>= runReaderT (Arkham.getTokenValue a iid token)

getCanAffordCost
  :: (MonadReader env m, HasGameRef env, HasQueue env, MonadIO m, HasStdGen env)
  => InvestigatorId
  -> Source
  -> Maybe Action
  -> Cost
  -> m Bool
getCanAffordCost iid source maction cost =
  toGameEnv >>= runReaderT (Helpers.getCanAffordCost iid source maction cost)

getModifiersFor
  :: ( MonadReader env m
     , HasGameRef env
     , MonadIO m
     , HasQueue env
     , HasStdGen env
     , HasModifiersFor GameEnv a
     )
  => Source
  -> Target
  -> a
  -> m [Modifier]
getModifiersFor s t a = toGameEnv >>= runReaderT (Arkham.getModifiersFor s t a)

data TestApp = TestApp
  { game :: IORef Game
  , messageQueueRef :: IORef [Message]
  , gen :: IORef StdGen
  , messageLogger :: Message -> IO ()
  }

newtype TestAppT m a = TestAppT { unTestAppT :: ReaderT TestApp m a }
  deriving newtype (MonadReader TestApp, Functor, Applicative, Monad, MonadTrans, MonadFail, MonadIO)

runTestApp :: TestApp -> TestAppT m a -> m a
runTestApp testApp = flip runReaderT testApp . unTestAppT

instance HasGameRef TestApp where
  gameRefL = lens game $ \m x -> m { game = x }

instance HasStdGen TestApp where
  genL = lens gen $ \m x -> m { gen = x }

instance HasQueue TestApp where
  messageQueue = lens messageQueueRef $ \m x -> m { messageQueueRef = x }

instance HasMessageLogger TestApp where
  messageLoggerL = lens messageLogger $ \m x -> m { messageLogger = x }

testScenario
  :: MonadIO m => CardCode -> (ScenarioAttrs -> ScenarioAttrs) -> m Scenario
testScenario cardCode f =
  let name = mkName $ unCardCode cardCode
  in pure $ baseScenario cardCode name [] [] Easy f

buildEvent :: MonadRandom m => CardCode -> Investigator -> m Event
buildEvent cardCode investigator =
  lookupEvent cardCode (toId investigator) <$> getRandom

buildEnemy :: MonadRandom m => CardCode -> m Enemy
buildEnemy cardCode = lookupEnemy cardCode <$> getRandom

buildAsset :: MonadRandom m => CardCode -> m Asset
buildAsset cardCode = lookupAsset cardCode <$> getRandom

testPlayerCards :: MonadIO m => Int -> m [PlayerCard]
testPlayerCards count' = replicateM count' (testPlayerCard id)

testPlayerCard :: MonadIO m => (CardDef -> CardDef) -> m PlayerCard
testPlayerCard f = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ MkPlayerCard
    { pcId = cardId
    , pcDef = f $ basePlayerCard "00000" "Test" 0 AssetType Guardian
    , pcBearer = Nothing
    }

buildPlayerCard :: MonadIO m => CardCode -> m PlayerCard
buildPlayerCard cardCode = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ lookupPlayerCard cardCode cardId

buildEncounterCard :: MonadIO m => CardCode -> m EncounterCard
buildEncounterCard cardCode = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ lookupEncounterCard cardCode cardId

buildTestEnemyEncounterCard :: MonadIO m => m EncounterCard
buildTestEnemyEncounterCard = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ lookupEncounterCard "enemy" cardId

buildTestTreacheryEncounterCard :: MonadIO m => m EncounterCard
buildTestTreacheryEncounterCard = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ lookupEncounterCard "treachery" cardId

testEnemy :: MonadRandom m => (EnemyAttrs -> EnemyAttrs) -> m Enemy
testEnemy f = do
  enemyId <- getRandom
  pure $ baseEnemy enemyId "enemy" f

testAsset :: MonadRandom m => (AssetAttrs -> AssetAttrs) -> m Asset
testAsset f = do
  assetId <- getRandom
  pure $ baseAsset assetId "asset" f

testAgenda :: MonadIO m => CardCode -> (AgendaAttrs -> AgendaAttrs) -> m Agenda
testAgenda cardCode f =
  pure $ baseAgenda (AgendaId cardCode) "Agenda" (Agenda 1 A) (Static 1) f

testLocation
  :: MonadRandom m => CardCode -> (LocationAttrs -> LocationAttrs) -> m Location
testLocation cardCode f = do
  locationId <- getRandom
  let name = Name (unCardCode cardCode) Nothing
  pure $ baseLocation locationId cardCode name 0 (Static 0) Square [] f

testInvestigator
  :: MonadIO m
  => CardCode
  -> (InvestigatorAttrs -> InvestigatorAttrs)
  -> m Investigator
testInvestigator cardCode f =
  let
    investigatorId = InvestigatorId cardCode
    name = unCardCode cardCode
    stats = Stats 5 5 5 5 5 5
  in pure $ baseInvestigator investigatorId name Neutral stats [] f

testConnectedLocations
  :: MonadRandom m
  => (LocationAttrs -> LocationAttrs)
  -> (LocationAttrs -> LocationAttrs)
  -> m (Location, Location)
testConnectedLocations f1 f2 = do
  location1 <- testLocation
    "00000"
    (f1
    . (symbolL .~ Square)
    . (revealedSymbolL .~ Square)
    . (connectedSymbolsL .~ setFromList [Triangle])
    . (revealedConnectedSymbolsL .~ setFromList [Triangle])
    )
  location2 <- testLocation
    "00001"
    (f2
    . (symbolL .~ Triangle)
    . (revealedSymbolL .~ Triangle)
    . (connectedSymbolsL .~ setFromList [Square])
    . (revealedConnectedSymbolsL .~ setFromList [Square])
    )
  pure (location1, location2)

testUnconnectedLocations
  :: MonadRandom m
  => (LocationAttrs -> LocationAttrs)
  -> (LocationAttrs -> LocationAttrs)
  -> m (Location, Location)
testUnconnectedLocations f1 f2 = do
  location1 <- testLocation
    "00000"
    (f1 . (symbolL .~ Square) . (revealedSymbolL .~ Square))
  location2 <- testLocation
    "00001"
    (f2 . (symbolL .~ Triangle) . (revealedSymbolL .~ Triangle))
  pure (location1, location2)

getActionsOf
  :: ( HasActions GameEnv a
     , TestEntity a
     , MonadIO m
     , MonadReader env m
     , HasStdGen env
     , HasGameRef env
     , HasQueue env
     )
  => Investigator
  -> Window
  -> a
  -> m [Message]
getActionsOf investigator window e = do
  e' <- updated e
  toGameEnv >>= runReaderT (getActions (toId investigator) window e')

getChaosBagTokens :: (HasGameRef env, MonadIO m, MonadReader env m) => m [Token]
getChaosBagTokens = view (chaosBagL . ChaosBag.tokensL) <$> getTestGame

createMessageMatcher :: MonadIO m => Message -> m (IORef Bool, Message -> m ())
createMessageMatcher msg = do
  ref <- liftIO $ newIORef False
  pure (ref, \msg' -> when (msg == msg') (liftIO $ atomicWriteIORef ref True))

didPassSkillTestBy
  :: MonadIO m
  => Investigator
  -> SkillType
  -> Int
  -> m (IORef Bool, Message -> m ())
didPassSkillTestBy investigator skillType n = createMessageMatcher
  (PassedSkillTest
    (toId investigator)
    Nothing
    (TestSource mempty)
    (SkillTestInitiatorTarget TestTarget)
    skillType
    n
  )

didFailSkillTestBy
  :: MonadIO m
  => Investigator
  -> SkillType
  -> Int
  -> m (IORef Bool, Message -> m ())
didFailSkillTestBy investigator skillType n = createMessageMatcher
  (FailedSkillTest
    (toId investigator)
    Nothing
    (TestSource mempty)
    (SkillTestInitiatorTarget TestTarget)
    skillType
    n
  )

withGame :: Game -> ReaderT Game m b -> m b
withGame = flip runReaderT

chooseOnlyOption
  :: ( MonadFail m
     , MonadIO m
     , HasQueue env
     , MonadReader env m
     , HasGameRef env
     , HasStdGen env
     , HasMessageLogger env
     )
  => String
  -> m ()
chooseOnlyOption _reason = do
  questionMap <- gameQuestion <$> getTestGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne [msg] -> unshiftMessage msg <* runMessages
      ChooseOneAtATime [msg] -> unshiftMessage msg <* runMessages
      ChooseN _ [msg] -> unshiftMessage msg <* runMessages
      _ -> error "spec expectation mismatch"
    _ -> error "There must be only one choice to use this function"

chooseFirstOption
  :: ( MonadFail m
     , MonadIO m
     , MonadReader env m
     , HasGameRef env
     , HasQueue env
     , HasStdGen env
     , HasMessageLogger env
     )
  => String
  -> m ()
chooseFirstOption _reason = do
  questionMap <- gameQuestion <$> getTestGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne (msg : _) -> unshiftMessage msg >> runMessages
      ChooseOneAtATime (msg : _) -> unshiftMessage msg >> runMessages
      _ -> error "spec expectation mismatch"
    _ -> error "There must be at least one option"

chooseOptionMatching
  :: ( MonadFail m
     , MonadIO m
     , MonadReader env m
     , HasGameRef env
     , HasQueue env
     , HasStdGen env
     , HasMessageLogger env
     )
  => String
  -> (Message -> Bool)
  -> m ()
chooseOptionMatching _reason f = do
  questionMap <- gameQuestion <$> getTestGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne msgs -> case find f msgs of
        Just msg -> unshiftMessage msg <* runMessages
        Nothing -> error "could not find a matching message"
      _ -> error "unsupported questions type"
    _ -> error "There must be only one question to use this function"

gameTest
  :: Investigator -> [Message] -> (Game -> Game) -> TestAppT IO () -> IO ()
gameTest = gameTestWithLogger (pure . const ())

gameTestWithLogger
  :: (Message -> IO ())
  -> Investigator
  -> [Message]
  -> (Game -> Game)
  -> TestAppT IO ()
  -> IO ()
gameTestWithLogger logger investigator queue f body = do
  g <- newGame investigator
  gameRef <- newIORef (f g)
  queueRef <- newIORef queue
  genRef <- newIORef $ mkStdGen (gameSeed g)
  runTestApp (TestApp gameRef queueRef genRef logger) body

newGame :: MonadIO m => Investigator -> m Game
newGame investigator = do
  scenario' <- testScenario "00000" id
  seed <- liftIO getRandom
  pure $ Game
    { gameRoundMessageHistory = []
    , gamePhaseMessageHistory = []
    , gameSeed = seed
    , gameInitialSeed = seed
    , gameMode = That scenario'
    , gamePlayerCount = 1
    , gameLocations = mempty
    , gameEnemies = mempty
    , gameEnemiesInVoid = mempty
    , gameAssets = mempty
    , gameInvestigators = HashMap.singleton investigatorId investigator
    , gamePlayers = HashMap.singleton 1 investigatorId
    , gameActiveInvestigatorId = investigatorId
    , gameLeadInvestigatorId = investigatorId
    , gamePhase = CampaignPhase -- TODO: maybe this should be a TestPhase or something?
    , gameEncounterDeck = mempty
    , gameDiscard = mempty
    , gameSkillTest = Nothing
    , gameAgendas = mempty
    , gameTreacheries = mempty
    , gameEvents = mempty
    , gameEffects = mempty
    , gameSkills = mempty
    , gameActs = mempty
    , gameChaosBag = emptyChaosBag
    , gameGameState = IsActive
    , gameResignedCardCodes = mempty
    , gameUsedAbilities = mempty
    , gameFocusedCards = mempty
    , gameFocusedTargets = mempty
    , gameFocusedTokens = mempty
    , gameActiveCard = Nothing
    , gamePlayerOrder = [investigatorId]
    , gamePlayerTurnOrder = [investigatorId]
    , gameVictoryDisplay = mempty
    , gameQuestion = mempty
    , gameHash = UUID.nil
    }
  where investigatorId = toId investigator

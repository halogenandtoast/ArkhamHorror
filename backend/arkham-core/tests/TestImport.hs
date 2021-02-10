{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestImport
  ( module X
  , module TestImport
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window
 as X

import Arkham.Game as X hiding (newGame)
import Arkham.Types.Agenda as X
import Arkham.Types.Asset as X
import Arkham.Types.ChaosBag as X
import Arkham.Types.Enemy as X
import Arkham.Types.Event as X
import Arkham.Types.Game as X
import Arkham.Types.Game.Helpers as X
import Arkham.Types.Investigator as X
import Arkham.Types.Location as X
import Arkham.Types.Scenario as X
import Arkham.Types.Stats as X
import Control.Lens as X (set, (^?!))
import Control.Monad.Fail as X
import Control.Monad.State as X (get)
import Data.UUID.V4 as X
import Helpers.Matchers as X
import Helpers.Message as X
import Test.Hspec as X

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card.PlayerCard (basePlayerCard)
import qualified Arkham.Types.ChaosBag as ChaosBag
import Arkham.Types.Difficulty
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Location.Attrs
import Arkham.Types.Phase
import Arkham.Types.Scenario.Attrs
import Control.Monad.State hiding (replicateM)
import qualified Data.HashMap.Strict as HashMap
import Data.These
import qualified Data.UUID as UUID

testScenario
  :: MonadIO m => CardCode -> (ScenarioAttrs -> ScenarioAttrs) -> m Scenario
testScenario cardCode f =
  let name = unCardCode cardCode
  in pure $ baseScenario cardCode name [] [] Easy f

insertEntity
  :: (Entity v, EntityId v ~ k, Eq k, Hashable k)
  => v
  -> HashMap k v
  -> HashMap k v
insertEntity a = insertMap (toId a) a

buildEvent :: MonadRandom m => CardCode -> Investigator -> m Event
buildEvent cardCode investigator =
  lookupEvent cardCode (toId investigator) <$> getRandom

buildEnemy :: MonadRandom m => CardCode -> m Enemy
buildEnemy cardCode = lookupEnemy cardCode <$> getRandom

buildAsset :: MonadRandom m => CardCode -> m Asset
buildAsset cardCode = lookupAsset cardCode <$> getRandom

testPlayerCards :: MonadIO m => Int -> m [PlayerCard]
testPlayerCards count' = replicateM count' (testPlayerCard id)

testPlayerCard :: MonadIO m => (PlayerCard -> PlayerCard) -> m PlayerCard
testPlayerCard f = do
  cardId <- CardId <$> liftIO nextRandom
  pure . f $ basePlayerCard cardId "00000" "Test" 0 AssetType Guardian

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
  :: MonadIO m => CardCode -> (LocationAttrs -> LocationAttrs) -> m Location
testLocation cardCode f =
  let
    locationId = LocationId cardCode
    name = Name (unCardCode cardCode) Nothing
  in pure $ baseLocation locationId name 0 (Static 0) Square [] f

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
  :: MonadIO m
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
  :: MonadIO m
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
  :: (HasActions GameInternal a, TestEntity a, MonadIO m)
  => GameExternal
  -> Investigator
  -> Window
  -> a
  -> m [Message]
getActionsOf game investigator window e =
  withGame game (getActions (toId investigator) window (updated game e))

chaosBagTokensOf :: Game queue -> [Token]
chaosBagTokensOf g = g ^. chaosBagL . ChaosBag.tokensL

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

withGame :: MonadIO m => GameExternal -> ReaderT GameInternal m b -> m b
withGame game f = toInternalGame game >>= runReaderT f

runGameTestOnlyOption
  :: (MonadFail m, MonadIO m, MonadRandom m)
  => String
  -> Game [Message]
  -> m (Game [Message])
runGameTestOnlyOption reason game =
  runGameTestOnlyOptionWithLogger reason (pure . const ()) game

runGameTestOnlyOptionWithLogger
  :: (MonadFail m, MonadIO m, MonadRandom m)
  => String
  -> (Message -> m ())
  -> Game [Message]
  -> m (Game [Message])
runGameTestOnlyOptionWithLogger _reason logger game =
  case mapToList (gameQuestion game) of
    [(_, question)] -> case question of
      ChooseOne [msg] ->
        toInternalGame (game { gameMessages = msg : gameMessages game })
          >>= runMessages logger
      ChooseOneAtATime [msg] ->
        toInternalGame (game { gameMessages = msg : gameMessages game })
          >>= runMessages logger
      ChooseN _ [msg] ->
        toInternalGame (game { gameMessages = msg : gameMessages game })
          >>= runMessages logger
      _ -> error "spec expectation mismatch"
    _ -> error "There must be only one choice to use this function"

runGameTestFirstOption
  :: (MonadFail m, MonadIO m, MonadRandom m)
  => String
  -> Game [Message]
  -> m (Game [Message])
runGameTestFirstOption _reason game = case mapToList (gameQuestion game) of
  [(_, question)] -> case question of
    ChooseOne (msg : _) ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages (pure . const ())
    ChooseOneAtATime (msg : _) ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages (pure . const ())
    _ -> error "spec expectation mismatch"
  _ -> error "There must be at least one option"

runGameTestMessages
  :: (MonadFail m, MonadIO m, MonadRandom m)
  => Game [Message]
  -> [Message]
  -> m (Game [Message])
runGameTestMessages game msgs =
  toInternalGame (game { gameMessages = msgs <> gameMessages game })
    >>= runMessages (pure . const ())

runGameTestOptionMatching
  :: (MonadFail m, MonadIO m, MonadRandom m)
  => String
  -> (Message -> Bool)
  -> Game [Message]
  -> m (Game [Message])
runGameTestOptionMatching reason f game =
  runGameTestOptionMatchingWithLogger reason (pure . const ()) f game

runGameTestOptionMatchingWithLogger
  :: (MonadFail m, MonadIO m, MonadRandom m)
  => String
  -> (Message -> m ())
  -> (Message -> Bool)
  -> Game [Message]
  -> m (Game [Message])
runGameTestOptionMatchingWithLogger _reason logger f game =
  case mapToList (gameQuestion game) of
    [(_, question)] -> case question of
      ChooseOne msgs -> case find f msgs of
        Just msg ->
          toInternalGame (game { gameMessages = msg : gameMessages game })
            >>= runMessages logger
        Nothing -> error "could not find a matching message"
      _ -> error "unsupported questions type"
    _ -> error "There must be only one question to use this function"

runGameTest
  :: (MonadIO m, MonadFail m, MonadRandom m)
  => Investigator
  -> [Message]
  -> (GameInternal -> GameInternal)
  -> m GameExternal
runGameTest investigator queue f =
  runGameTestWithLogger (pure . const ()) investigator queue f

runGameTestWithLogger
  :: (MonadIO m, MonadFail m, MonadRandom m)
  => (Message -> m ())
  -> Investigator
  -> [Message]
  -> (GameInternal -> GameInternal)
  -> m GameExternal
runGameTestWithLogger logger investigator queue f =
  newGame investigator queue >>= runMessages logger . f

newGame :: MonadIO m => Investigator -> [Message] -> m GameInternal
newGame investigator queue = do
  ref <- newIORef queue
  roundHistory <- newIORef []
  phaseHistory <- newIORef []
  scenario' <- testScenario "00000" id
  pure $ Game
    { gameMessages = ref
    , gameRoundMessageHistory = roundHistory
    , gamePhaseMessageHistory = phaseHistory
    , gameSeed = 1
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

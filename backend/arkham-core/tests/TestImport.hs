{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
module TestImport
  ( module X
  , module TestImport
  )
where

import Arkham.Types.Agenda as X
import qualified Arkham.Types.Agenda.Attrs as AgendaAttrs
import Arkham.Types.AgendaId as X
import Arkham.Types.Asset as X
import qualified Arkham.Types.Asset.Attrs as Asset
import Arkham.Types.AssetId as X
import Arkham.Types.Card as X
  ( Card(..)
  , CardCode(..)
  , EncounterCard
  , HasCardCode(..)
  , HasCardId(..)
  , PlayerCard
  , lookupEncounterCard
  , lookupPlayerCard
  )
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (basePlayerCard)
import Arkham.Types.Card.PlayerCard.Type as X
import Arkham.Types.ChaosBag as X
import qualified Arkham.Types.ChaosBag as ChaosBag
import Arkham.Types.Classes as X
import Arkham.Types.ClassSymbol
import Arkham.Types.Difficulty
import Arkham.Types.Enemy as X
import qualified Arkham.Types.Enemy.Attrs as Enemy
import Arkham.Types.EnemyId as X
import Arkham.Types.Event as X
import Arkham.Types.EventId as X
import Arkham.Types.Game as X
import Arkham.Types.GameValue as X
import Arkham.Types.Investigator as X
import qualified Arkham.Types.Investigator.Attrs as InvestigatorAttrs
import Arkham.Types.InvestigatorId as X
import Arkham.Types.Location as X
import qualified Arkham.Types.Location.Attrs as Location
import qualified Arkham.Types.Location.Attrs as LocationAttrs
import Arkham.Types.LocationId as X
import Arkham.Types.LocationSymbol
import Arkham.Types.Message as X
import Arkham.Types.Phase
import Arkham.Types.Scenario as X
import qualified Arkham.Types.Scenario.Attrs as ScenarioAttrs
import Arkham.Types.ScenarioId as X
import Arkham.Types.SkillType as X
import Arkham.Types.Source as X
import Arkham.Types.Stats as X
import Arkham.Types.Token
import Arkham.Types.Window
import ClassyPrelude as X
import Control.Monad.Fail as X
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID as UUID
import Data.UUID.V4 as X
import Helpers.Matchers as X
import Helpers.Message as X
import Lens.Micro as X
import Test.Hspec as X

testScenario
  :: MonadIO m
  => CardCode
  -> (ScenarioAttrs.Attrs -> ScenarioAttrs.Attrs)
  -> m Scenario
testScenario cardCode f =
  let name = unCardCode cardCode
  in pure $ baseScenario cardCode name [] [] Easy f

insertEntity
  :: (HasId k () v, Eq k, Hashable k) => v -> HashMap k v -> HashMap k v
insertEntity a = insertMap (getId () a) a

buildEvent :: MonadIO m => CardCode -> Investigator -> m Event
buildEvent cardCode investigator = do
  eventId <- liftIO $ EventId <$> nextRandom
  pure $ lookupEvent cardCode (getId () investigator) eventId

buildEnemy :: MonadIO m => CardCode -> m Enemy
buildEnemy cardCode = do
  enemyId <- liftIO $ EnemyId <$> nextRandom
  pure $ lookupEnemy cardCode enemyId

buildAsset :: MonadIO m => CardCode -> m Asset
buildAsset cardCode = do
  assetId <- liftIO $ AssetId <$> nextRandom
  pure $ lookupAsset cardCode assetId

testPlayerCards :: MonadIO m => Int -> m [PlayerCard]
testPlayerCards count' = replicateM count' testPlayerCard

testPlayerCard :: MonadIO m => m PlayerCard
testPlayerCard = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ basePlayerCard cardId "asset" "Test" 0 AssetType Guardian

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

testEnemy :: MonadIO m => (Enemy.Attrs -> Enemy.Attrs) -> m Enemy
testEnemy f = do
  enemyId <- liftIO $ EnemyId <$> nextRandom
  pure $ baseEnemy enemyId "enemy" f

testAsset :: MonadIO m => (Asset.Attrs -> Asset.Attrs) -> m Asset
testAsset f = do
  assetId <- liftIO $ AssetId <$> nextRandom
  pure $ baseAsset assetId "asset" f

testAgenda
  :: MonadIO m
  => CardCode
  -> (AgendaAttrs.Attrs -> AgendaAttrs.Attrs)
  -> m Agenda
testAgenda cardCode f =
  pure $ baseAgenda (AgendaId cardCode) "Agenda" "1A" (Static 1) f

testLocation
  :: MonadIO m
  => CardCode
  -> (LocationAttrs.Attrs -> LocationAttrs.Attrs)
  -> m Location
testLocation cardCode f =
  let
    locationId = LocationId cardCode
    name = unCardCode cardCode
  in pure $ baseLocation locationId name 0 (Static 0) Square [] f

testInvestigator
  :: MonadIO m
  => CardCode
  -> (InvestigatorAttrs.Attrs -> InvestigatorAttrs.Attrs)
  -> m Investigator
testInvestigator cardCode f =
  let
    investigatorId = InvestigatorId cardCode
    name = unCardCode cardCode
    stats = Stats 5 5 5 5 5 5
  in pure $ baseInvestigator investigatorId name Neutral stats [] f

testConnectedLocations
  :: MonadIO m
  => (LocationAttrs.Attrs -> LocationAttrs.Attrs)
  -> (LocationAttrs.Attrs -> LocationAttrs.Attrs)
  -> m (Location, Location)
testConnectedLocations f1 f2 = do
  location1 <- testLocation
    "00000"
    (f1
    . (Location.symbol .~ Square)
    . (Location.revealedSymbol .~ Square)
    . (Location.connectedSymbols .~ setFromList [Triangle])
    . (Location.revealedConnectedSymbols .~ setFromList [Triangle])
    )
  location2 <- testLocation
    "00001"
    (f2
    . (Location.symbol .~ Triangle)
    . (Location.revealedSymbol .~ Triangle)
    . (Location.connectedSymbols .~ setFromList [Square])
    . (Location.revealedConnectedSymbols .~ setFromList [Square])
    )
  pure (location1, location2)

testUnconnectedLocations
  :: MonadIO m
  => (LocationAttrs.Attrs -> LocationAttrs.Attrs)
  -> (LocationAttrs.Attrs -> LocationAttrs.Attrs)
  -> m (Location, Location)
testUnconnectedLocations f1 f2 = do
  location1 <- testLocation
    "00000"
    (f1 . (Location.symbol .~ Square) . (Location.revealedSymbol .~ Square))
  location2 <- testLocation
    "00001"
    (f2 . (Location.symbol .~ Triangle) . (Location.revealedSymbol .~ Triangle))
  pure (location1, location2)

getActionsOf
  :: (MonadIO m, HasActions GameInternal investigator a)
  => GameExternal
  -> investigator
  -> Window
  -> a
  -> m [Message]
getActionsOf game investigator window e =
  withGame game (getActions investigator window e)

chaosBagTokensOf :: Game queue -> [Token]
chaosBagTokensOf g = g ^. chaosBag . ChaosBag.chaosBagTokensLens

withGame :: MonadIO m => GameExternal -> ReaderT GameInternal m b -> m b
withGame game f = toInternalGame game >>= runReaderT f

runGameTestOnlyOption
  :: (MonadFail m, MonadIO m) => String -> Game [Message] -> m (Game [Message])
runGameTestOnlyOption _reason game = case mapToList (gameQuestion game) of
  [(_, question)] -> case question of
    ChooseOne [msg] ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages
    ChooseOneAtATime [msg] ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages
    _ -> error "spec expectation mismatch"
  _ -> error "There must be only one choice to use this function"

runGameTestFirstOption
  :: (MonadFail m, MonadIO m) => String -> Game [Message] -> m (Game [Message])
runGameTestFirstOption _reason game = case mapToList (gameQuestion game) of
  [(_, question)] -> case question of
    ChooseOne (msg : _) ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages
    ChooseOneAtATime (msg : _) ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages
    _ -> error "spec expectation mismatch"
  _ -> error "There must be at least one option"

runGameTestMessages
  :: (MonadFail m, MonadIO m)
  => Game [Message]
  -> [Message]
  -> m (Game [Message])
runGameTestMessages game msgs =
  toInternalGame (game { gameMessages = msgs <> gameMessages game })
    >>= runMessages

runGameTestOptionMatching
  :: (MonadFail m, MonadIO m)
  => String
  -> (Message -> Bool)
  -> Game [Message]
  -> m (Game [Message])
runGameTestOptionMatching _reason f game =
  case mapToList (gameQuestion game) of
    [(_, question)] -> case question of
      ChooseOne msgs -> case find f msgs of
        Just msg ->
          toInternalGame (game { gameMessages = msg : gameMessages game })
            >>= runMessages
        Nothing -> error "could not find a matching message"
      _ -> error "unsupported questions type"
    _ -> error "There must be only one question to use this function"

runGameTest
  :: Investigator
  -> [Message]
  -> (Game (IORef [Message]) -> Game (IORef [Message]))
  -> IO (Game [Message])
runGameTest investigator queue f =
  newGame investigator queue >>= runMessages . f

newGame :: MonadIO m => Investigator -> [Message] -> m (Game (IORef [Message]))
newGame investigator queue = do
  ref <- newIORef queue
  history <- newIORef []
  pure $ Game
    { gameMessages = ref
    , gameMessageHistory = history
    , gameSeed = 1
    , gameCampaign = Nothing
    , gameScenario = Nothing
    , gamePlayerCount = 1
    , gameLocations = mempty
    , gameEnemies = mempty
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
    , gameSkills = mempty
    , gameActs = mempty
    , gameChaosBag = emptyChaosBag
    , gameGameOver = False
    , gamePending = False
    , gameUsedAbilities = mempty
    , gameFocusedCards = mempty
    , gameFocusedTokens = mempty
    , gameActiveCard = Nothing
    , gamePlayerOrder = [investigatorId]
    , gameVictoryDisplay = mempty
    , gameQuestion = mempty
    , gameHash = UUID.nil
    }
  where investigatorId = getInvestigatorId investigator

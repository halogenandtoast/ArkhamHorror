{-# LANGUAGE OverloadedRecordDot #-}

module Api.Arkham.Helpers where

import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes hiding (Entity (..), select)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Debug
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.Queue
import Arkham.Random
import Arkham.Tracing
import Control.Concurrent.MVar
import Control.Lens hiding (from)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Random (MonadRandom (..), StdGen)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Database.Redis (RedisChannel)
import Entity.Arkham.LogEntry
import GHC.Records
import Import hiding (appLogger, appTracer, (==.), (>=.))
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad (inSpan', MonadTracer (..))

newtype GameLog = GameLog {gameLogToLogEntries :: [Text]}
  deriving newtype (Monoid, Semigroup)

instance HasField "entries" GameLog [Text] where
  getField = gameLogToLogEntries

newLogEntry :: ArkhamGameId -> Int -> UTCTime -> Text -> ArkhamLogEntry
newLogEntry gameId step now body =
  ArkhamLogEntry
    { arkhamLogEntryBody = body
    , arkhamLogEntryArkhamGameId = gameId
    , arkhamLogEntryStep = step
    , arkhamLogEntryCreatedAt = now
    }

getGameLog :: MonadIO m => ArkhamGameId -> Maybe Int -> SqlPersistT m GameLog
getGameLog gameId mStep = fmap (GameLog . fmap unValue) $ select $ do
  entries <- from $ table @ArkhamLogEntry
  where_ $ entries.arkhamGameId ==. val gameId
  for_ mStep \step ->
    where_ $ entries.step >=. val step
  orderBy [asc entries.createdAt]
  pure entries.body

toPublicGame :: Entity ArkhamGame -> GameLog -> PublicGame ArkhamGameId
toPublicGame (Entity gId ArkhamGame {..}) gameLog =
  PublicGame gId arkhamGameName (gameLogToLogEntries gameLog) arkhamGameCurrentData

data ApiResponse
  = GameUpdate (PublicGame ArkhamGameId)
  | GameMessage Text
  | GameError Text
  | GameCard {title :: Text, card :: Aeson.Value}
  | GameCardOnly {player :: PlayerId, title :: Text, card :: Aeson.Value}
  | GameTarot Aeson.Value
  | GameShowDiscard InvestigatorId
  | GameShowUnder InvestigatorId
  deriving stock Generic
  deriving anyclass ToJSON

newtype GameAppT a = GameAppT {unGameAppT :: ReaderT GameApp IO a}
  deriving newtype
    ( MonadReader GameApp
    , Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , MonadRandom
    , MonadMask
    , MonadCatch
    , MonadThrow
    , MonadUnliftIO
    )

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: Queue Message
  , appGen :: IORef StdGen
  , appLogger :: ClientMessage -> IO ()
  , appTracer :: Trace.Tracer
  }

instance HasDebugLevel GameAppT where
  getDebugLevel = liftIO getDebugLevel

instance HasGame GameAppT where
  getGame = readIORef =<< asks appGame
  getCache = GameCache \_ build -> build

overGameCards :: (Map CardId Card -> Map CardId Card) -> GameAppT ()
overGameCards f = do
  ref <- asks appGame
  atomicModifyIORef' ref \g -> (g {gameCards = f (gameCards g)}, ())

instance CardGen GameAppT where
  genEncounterCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let card = lookupEncounterCard (toCardDef a) cardId
    overGameCards $ Map.insert cardId (toCard card)
    pure card
  genPlayerCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let card = lookupPlayerCard (toCardDef a) cardId
    overGameCards $ Map.insert cardId (toCard card)
    pure card
  replaceCard cardId card = overGameCards $ Map.insert cardId card
  removeCard cardId = overGameCards $ Map.delete cardId
  clearCardCache = overGameCards $ Map.filter (not . isEncounterCard)

instance HasStdGen GameApp where
  genL = lens appGen $ \m x -> m {appGen = x}

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m {appGame = x}

instance HasQueue Message GameAppT where
  messageQueue = asks appQueue

instance MonadTracer GameAppT where
  getTracer = asks appTracer

instance HasGameLogger GameAppT where
  getLogger = do
    logger <- asks appLogger
    pure $ \msg -> liftIO $ logger msg

instance Tracing GameAppT where
  type SpanType GameAppT = Trace.Span
  type SpanArgs GameAppT = Trace.SpanArguments
  addAttribute = Trace.addAttribute
  defaultSpanArgs = Trace.defaultSpanArguments
  doTrace name args action = inSpan' name args action

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

noLogger :: Applicative m => Text -> m ()
noLogger = const (pure ())

gameChannel :: ArkhamGameId -> RedisChannel
gameChannel gameId = "arkham-" <> encodeUtf8 (tshow gameId)

getRoom :: (MonadIO m, HasApp m) => ArkhamGameId -> m Room
getRoom gid = do
  roomsVar <- getsApp appGameRooms
  liftIO do
    modifyMVar roomsVar \rooms -> do
      case Map.lookup gid rooms of
        Just r -> pure (rooms, r)
        Nothing -> do
          chan <- atomically newBroadcastTChan
          let r =
                Room
                  { socketChannel = chan
                  , socketClients = 0
                  , messageBrokerChannel = gameChannel gid
                  }
          pure (Map.insert gid r rooms, r)

displayCardType :: CardType -> Text
displayCardType = \case
  ActType -> "act"
  AgendaType -> "act"
  AssetType -> "asset"
  EventType -> "event"
  ScenarioType -> "scenario"
  SkillType -> "skill"
  StoryType -> "story"
  PlayerTreacheryType -> "treachery"
  PlayerEnemyType -> "enemy"
  TreacheryType -> "treachery"
  EnemyType -> "enemy"
  LocationType -> "location"
  EncounterAssetType -> "asset"
  EncounterEventType -> "event"
  InvestigatorType -> "investigator"
  KeyType -> "key"

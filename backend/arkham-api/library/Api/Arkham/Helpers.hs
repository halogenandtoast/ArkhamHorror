{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Api.Arkham.Helpers where

import Import hiding (appLogger, (==.), (>=.))

import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes hiding (Entity (..), select)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Game
import Arkham.Message
import Control.Lens hiding (from)
import Control.Monad.Random (MonadRandom (..), StdGen, mkStdGen)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Entity.Arkham.LogEntry

newtype GameLog = GameLog {gameLogToLogEntries :: [Text]}
  deriving newtype (Monoid, Semigroup)

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
  for_ mStep $ \step ->
    where_ $ entries.step >=. val step
  orderBy [asc entries.createdAt]
  pure $ entries.body

getGameLogEntries :: MonadIO m => ArkhamGameId -> SqlPersistT m [ArkhamLogEntry]
getGameLogEntries gameId = fmap (fmap entityVal)
  . select
  $ do
    entries <- from $ table @ArkhamLogEntry
    where_ $ entries.arkhamGameId ==. val gameId
    orderBy [asc entries.createdAt]
    pure entries

toPublicGame :: Entity ArkhamGame -> GameLog -> PublicGame ArkhamGameId
toPublicGame (Entity gId ArkhamGame {..}) gameLog =
  PublicGame gId arkhamGameName (gameLogToLogEntries gameLog) arkhamGameCurrentData

data ApiResponse
  = GameUpdate (PublicGame ArkhamGameId)
  | GameMessage Text
  | GameError Text
  | GameCard {title :: Text, card :: Aeson.Value}
  | GameTarot Aeson.Value
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype GameAppT a = GameAppT {unGameAppT :: ReaderT GameApp IO a}
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom)

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: Queue Message
  , appGen :: IORef StdGen
  , appLogger :: ClientMessage -> IO ()
  }

instance HasDebugLevel GameAppT where
  getDebugLevel = liftIO getDebugLevel

instance HasGame GameAppT where
  getGame = readIORef =<< asks appGame

instance CardGen GameAppT where
  genEncounterCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let !card = lookupEncounterCard (toCardDef a) cardId
    ref <- asks appGame
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = Map.insert cardId (EncounterCard card) (gameCards g)}, ())
    pure card
  genPlayerCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let !card = lookupPlayerCard (toCardDef a) cardId
    ref <- asks appGame
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = Map.insert cardId (PlayerCard card) (gameCards g)}, ())
    pure card
  replaceCard cardId card = do
    ref <- asks appGame
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = Map.insert cardId card (gameCards g)}, ())

newApp :: MonadIO m => Game -> (ClientMessage -> IO ()) -> [Message] -> m GameApp
newApp g logger msgs = do
  gameRef <- newIORef g
  queueRef <- newQueue msgs
  genRef <- newIORef (mkStdGen (gameSeed g))
  pure $ GameApp gameRef queueRef genRef logger

instance HasStdGen GameApp where
  genL = lens appGen $ \m x -> m {appGen = x}

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m {appGame = x}

instance HasQueue Message GameAppT where
  messageQueue = asks appQueue

instance HasGameLogger GameAppT where
  getLogger = do
    logger <- asks appLogger
    pure $ \msg -> liftIO $ logger msg

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

noLogger :: Applicative m => Text -> m ()
noLogger = const (pure ())

getChannel :: ArkhamGameId -> Handler (TChan BSL.ByteString)
getChannel gameId = do
  gameChannelsRef <- appGameChannels <$> getYesod
  gameChannels <- readIORef gameChannelsRef
  case Map.lookup gameId gameChannels of
    Just chan -> pure chan
    Nothing -> do
      chan <- atomically newBroadcastTChan
      atomicModifyIORef' gameChannelsRef
        $ \gameChannels' -> (Map.insert gameId chan gameChannels', ())
      pure chan

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

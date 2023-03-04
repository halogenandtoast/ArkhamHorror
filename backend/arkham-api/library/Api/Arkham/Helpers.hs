{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Api.Arkham.Helpers where

import Import hiding ( appLogger, (==.), (>=.) )

import Arkham.Card
import Arkham.Classes hiding ( Entity (..), select )
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.PlayerCard
import Control.Lens hiding (from)
import Control.Monad.Random ( MonadRandom (..), StdGen, mkStdGen )
import Data.Aeson.Key ( fromText )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Json
import Text.Parsec ( char, digit, many1, sepBy, parse, ParsecT )
import Data.IntMap qualified as IntMap
import Text.Read (read)
import Entity.Arkham.LogEntry
import Database.Esqueleto.Experimental
import Data.Time.Clock

type Parser = ParsecT Text () Identity

newtype GameLog = GameLog { gameLogToLogEntries :: [Text] }
  deriving newtype (Monoid, Semigroup)

newLogEntry :: ArkhamGameId -> Int -> UTCTime -> Text -> ArkhamLogEntry
newLogEntry gameId step now body = ArkhamLogEntry
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
getGameLogEntries gameId = fmap (fmap entityVal) . select $ do
  entries <- from $ table @ArkhamLogEntry
  where_ $ entries.arkhamGameId ==. val gameId
  orderBy [asc entries.createdAt]
  pure entries

toPublicGame :: Entity ArkhamGame -> GameLog -> PublicGame ArkhamGameId
toPublicGame (Entity gId ArkhamGame {..}) gameLog =
  PublicGame gId arkhamGameName (gameLogToLogEntries gameLog) arkhamGameCurrentData

data ApiResponse = GameUpdate (PublicGame ArkhamGameId) | GameMessage Text
  deriving stock Generic
  deriving anyclass ToJSON

newtype GameAppT a = GameAppT { unGameAppT :: ReaderT GameApp IO a }
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom)

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: Queue Message
  , appGen :: IORef StdGen
  , appLogger :: Text -> IO ()
  }

newApp :: MonadIO m => Game -> (Text -> IO ()) -> [Message] -> m GameApp
newApp g logger msgs = do
  gameRef <- newIORef g
  queueRef <- newQueue msgs
  genRef <- newIORef (mkStdGen (gameSeed g))
  pure $ GameApp gameRef queueRef genRef logger

instance HasStdGen GameApp where
  genL = lens appGen $ \m x -> m { appGen = x }

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m { appGame = x }

instance HasQueue Message GameAppT where
  messageQueue = asks appQueue

instance HasGameLogger GameApp where
  gameLoggerL = lens appLogger $ \m x -> m { appLogger = x }

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

newtype ArkhamDBDecklistMeta = ArkhamDBDecklistMeta
  { alternate_front :: InvestigatorId
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadDecklistCards :: (CardGen m) => ArkhamDBDecklist -> m [PlayerCard]
loadDecklistCards decklist = do
  results <- forM (HashMap.toList $ slots decklist) $ \(cardCode, count') ->
    replicateM count' (applyCustomizations decklist <$> genPlayerCard (lookupPlayerCardDef cardCode))
  pure $ fold results

applyCustomizations :: ArkhamDBDecklist -> PlayerCard -> PlayerCard
applyCustomizations deckList pCard = case meta deckList of
  Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
    Just (Object o) ->
      case KeyMap.lookup (fromText $ "cus_" <> unCardCode (pcCardCode pCard)) o of
        Just (fromJSON -> Success customizations) -> case parse parseCustomizations "" customizations of
          Left _ -> pCard
          Right cs -> pCard { pcCustomizations = cs }
        _ -> pCard
    _ -> pCard
  _ -> pCard
 where
  parseCustomizations :: Parser (IntMap Int)
  parseCustomizations = do
    let parseInt = read <$> many1 digit
    IntMap.fromList <$> sepBy ((,) <$> parseInt <*> (char '|' *> parseInt)) (char ',')

loadDecklist :: CardGen m => ArkhamDeck -> m (InvestigatorId, [PlayerCard])
loadDecklist arkhamDeck = (investigatorId, ) <$> loadDecklistCards decklist
 where
  decklist = arkhamDeckList arkhamDeck
  investigatorId = case meta decklist of
    Nothing -> investigator_code decklist
    Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
      Nothing -> investigator_code decklist
      Just ArkhamDBDecklistMeta {..} -> if alternate_front == ""
        then investigator_code decklist
        else alternate_front

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
  InvestigatorType -> "investigator"

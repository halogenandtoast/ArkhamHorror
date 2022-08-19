{-# LANGUAGE TupleSections #-}
module Api.Arkham.Helpers where

import Import hiding ( appLogger )

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes hiding ( Entity (..) )
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.PlayerCard
import Control.Lens
import Control.Monad.Random ( MonadRandom (..), StdGen, mkStdGen )
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Json

toPublicGame :: Entity ArkhamGame -> PublicGame ArkhamGameId
toPublicGame (Entity gId ArkhamGame {..}) =
  PublicGame gId arkhamGameName arkhamGameLog arkhamGameCurrentData

data ApiResponse = GameUpdate (PublicGame ArkhamGameId) | GameMessage Text
  deriving stock Generic
  deriving anyclass ToJSON

newtype GameAppT a = GameAppT { unGameAppT :: ReaderT GameApp IO a }
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom)

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: IORef [Message]
  , appGen :: IORef StdGen
  , appLogger :: Text -> IO ()
  }

newApp :: MonadIO m => Game -> (Text -> IO ()) -> [Message] -> m GameApp
newApp g logger msgs = do
  gameRef <- newIORef g
  queueRef <- newIORef msgs
  genRef <- newIORef (mkStdGen (gameSeed g))
  pure $ GameApp gameRef queueRef genRef logger

instance HasStdGen GameApp where
  genL = lens appGen $ \m x -> m { appGen = x }

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m { appGame = x }

instance HasQueue GameApp where
  messageQueue = lens appQueue $ \m x -> m { appQueue = x }

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

loadDecklistCards :: ArkhamDBDecklist -> IO [PlayerCard]
loadDecklistCards decklist =
  flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
    replicateM count' (genPlayerCard (lookupPlayerCardDef cardCode))

loadDecklist :: ArkhamDeck -> IO (InvestigatorId, [PlayerCard])
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
  SkillType -> "skill"
  StoryType -> "story"
  PlayerTreacheryType -> "treachery"
  PlayerEnemyType -> "enemy"
  TreacheryType -> "treachery"
  EnemyType -> "enemy"
  LocationType -> "location"
  EncounterAssetType -> "asset"
  InvestigatorType -> "investigator"

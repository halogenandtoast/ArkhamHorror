{-# LANGUAGE TupleSections #-}
module Api.Arkham.Helpers where

import Arkham.PlayerCard
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes.HasQueue
import Arkham.Types.Game
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Control.Lens
import Control.Monad.Fail
import Control.Monad.Random (MonadRandom(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.UUID.V4
import Import
import Json

newtype GameAppT a = GameAppT { unGameAppT :: ReaderT GameApp IO a }
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom)

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: IORef [Message]
  }

-- instance MonadRandom m => MonadRandom (GameAppT m) where
--   getRandomR = lift . getRandomR
--   getRandom = lift getRandom
--   getRandomRs = lift . getRandomRs
--   getRandoms = lift getRandoms

newApp :: MonadIO m => Game -> [Message] -> m GameApp
newApp g msgs = do
  gameRef <- newIORef g
  queueRef <- newIORef msgs
  pure $ GameApp gameRef queueRef

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m { appGame = x }

instance HasQueue GameApp where
  messageQueue = lens appQueue $ \m x -> m { appQueue = x }

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

noLogger :: Applicative m => Message -> m ()
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

newtype ArkhamDBDecklistMeta = ArkhamDBDecklistMeta { alternate_front :: InvestigatorId }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadDecklistCards :: ArkhamDBDecklist -> IO [PlayerCard]
loadDecklistCards decklist =
  flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
    if cardCode /= "01000"
      then replicateM
        count'
        ((<$> (CardId <$> nextRandom)) (lookupPlayerCard cardCode))
      else pure []

loadDecklist :: ArkhamDeck -> IO (InvestigatorId, [PlayerCard])
loadDecklist arkhamDeck = (investigatorId, ) <$> loadDecklistCards decklist
 where
  decklist = arkhamDeckList arkhamDeck
  investigatorId = case meta decklist of
    Nothing -> investigator_code decklist
    Just meta' ->
      case decode @ArkhamDBDecklistMeta (encodeUtf8 $ fromStrict meta') of
        Nothing -> investigator_code decklist
        Just ArkhamDBDecklistMeta {..} -> alternate_front

toHumanReadable :: Message -> Maybe Text
toHumanReadable = \case
  BeginEnemy -> Just "Begin enemy phase"
  BeginInvestigation -> Just "Begin investigation phase"
  BeginMythos -> Just "Begin mythos phase"
  BeginUpkeep -> Just "Begin upkeep phase"
  msg -> Just $ tshow msg

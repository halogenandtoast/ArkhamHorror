{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Api.Arkham.Helpers where

import Import hiding ( appLogger )

import Arkham.Card
import Arkham.Classes hiding ( Entity (..) )
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Arkham.PlayerCard
import Control.Lens
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

type Parser = ParsecT Text () Identity

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
  TreacheryType -> "treachery"
  EnemyType -> "enemy"
  LocationType -> "location"
  InvestigatorType -> "investigator"

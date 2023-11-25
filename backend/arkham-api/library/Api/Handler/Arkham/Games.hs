{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games (
  getApiV1ArkhamGameR,
  getApiV1ArkhamGameSpectateR,
  getApiV1ArkhamGamesR,
  postApiV1ArkhamGamesR,
  putApiV1ArkhamGameR,
  deleteApiV1ArkhamGameR,
  putApiV1ArkhamGameRawR,
) where

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Difficulty
import Arkham.Game
import Arkham.Game.Diff
import Arkham.GameEnv
import Arkham.Id
import Arkham.Message
import Conduit
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Control.Monad.Random.Class (getRandom)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update)
import Entity.Answer
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Import qualified as P
import Json
import Network.WebSockets (ConnectionException)
import Safe (fromJustNote)
import UnliftIO.Exception hiding (Handler)
import Yesod.WebSockets

gameStream :: Maybe UserId -> ArkhamGameId -> WebSocketsT Handler ()
gameStream mUserId gameId = catchingConnectionException $ do
  writeChannel <- lift $ getChannel gameId
  gameChannelClients <- appGameChannelClients <$> getYesod
  atomicModifyIORef' gameChannelClients
    $ \channelClients -> (Map.insertWith (+) gameId 1 channelClients, ())
  bracket (atomically $ dupTChan writeChannel) closeConnection
    $ \readChannel ->
      race_
        (forever $ atomically (readTChan readChannel) >>= sendTextData)
        (runConduit $ sourceWS .| mapM_C (handleData writeChannel))
 where
  handleData writeChannel dataPacket = lift $ do
    for_ mUserId $ \userId ->
      case eitherDecodeStrict dataPacket of
        Left err -> $(logWarn) $ tshow err
        Right answer -> updateGame answer gameId userId writeChannel

  closeConnection _ = do
    gameChannelsRef <- appGameChannels <$> lift getYesod
    gameChannelClientsRef <- appGameChannelClients <$> lift getYesod
    clientCount <-
      atomicModifyIORef' gameChannelClientsRef $ \channelClients ->
        ( Map.adjust pred gameId channelClients
        , Map.findWithDefault 1 gameId channelClients - 1
        )
    when (clientCount == 0)
      $ atomicModifyIORef' gameChannelsRef
      $ \gameChannels' -> (Map.delete gameId gameChannels', ())

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ tshow (e :: ConnectionException)

data GetGameJson = GetGameJson
  { playerId :: Maybe PlayerId
  , multiplayerMode :: MultiplayerVariant
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

getApiV1ArkhamGameR :: HasCallStack => ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  webSockets $ gameStream (Just userId) gameId
  ge <- runDB $ get404 gameId
  Entity playerId _ <- runDB $ getBy404 (UniquePlayer userId gameId)
  let Game {..} = arkhamGameCurrentData ge
  gameLog <- runDB $ getGameLog gameId Nothing
  let
    player =
      case arkhamGameMultiplayerVariant ge of
        WithFriends -> coerce playerId
        Solo -> gameActivePlayerId
  pure
    $ GetGameJson
      (Just player)
      (arkhamGameMultiplayerVariant ge)
      (PublicGame gameId (arkhamGameName ge) (gameLogToLogEntries gameLog) (arkhamGameCurrentData ge))

getApiV1ArkhamGameSpectateR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameSpectateR gameId = do
  webSockets $ gameStream Nothing gameId
  ge <- runDB $ get404 gameId
  let Game {..} = arkhamGameCurrentData ge
  gameLog <- runDB $ getGameLog gameId Nothing
  pure
    $ GetGameJson
      (Just gameActivePlayerId)
      (arkhamGameMultiplayerVariant ge)
      (PublicGame gameId (arkhamGameName ge) (gameLogToLogEntries gameLog) (arkhamGameCurrentData ge))

getApiV1ArkhamGamesR :: Handler [PublicGame ArkhamGameId]
getApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  games <- runDB $ select $ do
    (players :& games) <-
      distinct
        $ from
        $ table @ArkhamPlayer
        `innerJoin` table @ArkhamGame
          `on` (\(players :& games) -> players.arkhamGameId ==. games.id)
    where_ $ players.userId ==. val userId
    orderBy [desc $ games.updatedAt]
    pure games
  pure $ map (`toPublicGame` mempty) games

data CreateGamePost = CreateGamePost
  { deckIds :: [Maybe ArkhamDeckId]
  , playerCount :: Int
  , campaignId :: Maybe CampaignId
  , scenarioId :: Maybe ScenarioId
  , difficulty :: Difficulty
  , campaignName :: Text
  , multiplayerVariant :: MultiplayerVariant
  , includeTarotReadings :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | New Game
postApiV1ArkhamGamesR :: Handler (PublicGame ArkhamGameId)
postApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  CreateGamePost {..} <- requireCheckJsonBody

  newGameSeed <- liftIO getRandom
  genRef <- newIORef (mkStdGen newGameSeed)
  queueRef <- newQueue []
  now <- liftIO getCurrentTime

  let
    game = case campaignId of
      Just cid -> newCampaign cid scenarioId newGameSeed playerCount difficulty includeTarotReadings
      Nothing -> case scenarioId of
        Just sid -> newScenario sid newGameSeed playerCount difficulty includeTarotReadings
        Nothing -> error "missing either a campign id or a scenario id"
  let ag = ArkhamGame campaignName game 0 multiplayerVariant now now
  let repeatCount = if multiplayerVariant == WithFriends then 1 else playerCount
  (key, pids) <- runDB $ do
    gameId <- insert ag
    pids <- insertMany $ replicate repeatCount $ ArkhamPlayer userId gameId "00000"
    pure (gameId, pids)

  gameRef <- newIORef game

  runGameApp (GameApp gameRef queueRef genRef (pure . const ())) $ do
    for_ pids $ \pid -> addPlayer (PlayerId $ coerce pid)
    runMessages Nothing

  updatedQueue <- readIORef (queueToRef queueRef)
  updatedGame <- readIORef gameRef

  let ag' = ag {arkhamGameCurrentData = updatedGame}

  -- let diffDown = diff ge game

  runDB $ do
    replace key ag'
    insert_ $ ArkhamStep key (Choice mempty updatedQueue) 0 (ActionDiff [])
  pure $ toPublicGame (Entity key ag') mempty

putApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  response <- requireCheckJsonBody
  writeChannel <- getChannel gameId
  updateGame response gameId userId writeChannel

updateGame :: Answer -> ArkhamGameId -> UserId -> TChan BSL.ByteString -> Handler ()
updateGame response gameId userId writeChannel = do
  (Entity _ _, ArkhamGame {..}) <-
    runDB
      $ (,)
      <$> getBy404 (UniquePlayer userId gameId)
      <*> get404 gameId
  mLastStep <- runDB $ getBy $ UniqueStep gameId arkhamGameStep
  let
    gameJson@Game {..} = arkhamGameCurrentData
    currentQueue =
      maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

  activePlayer <- runReaderT getActivePlayer gameJson

  let playerId = fromMaybe activePlayer (answerPlayer response)

  messages <- handleAnswer gameJson playerId response
  gameRef <- newIORef gameJson
  queueRef <- newQueue ((ClearUI : messages) <> currentQueue)
  logRef <- newIORef []
  genRef <- newIORef $ mkStdGen gameSeed

  runGameApp
    (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel))
    (runMessages Nothing)

  ge <- readIORef gameRef
  let diffDown = diff ge arkhamGameCurrentData

  oldLog <- runDB $ getGameLog gameId Nothing
  updatedQueue <- readIORef $ queueToRef queueRef
  updatedLog <- readIORef logRef

  now <- liftIO getCurrentTime
  runDB $ do
    replace gameId
      $ ArkhamGame
        arkhamGameName
        ge
        (arkhamGameStep + 1)
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now
    insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog
    deleteWhere [ArkhamStepArkhamGameId P.==. gameId, ArkhamStepStep P.>. arkhamGameStep]
    insert_
      $ ArkhamStep
        gameId
        (Choice diffDown updatedQueue)
        (arkhamGameStep + 1)
        (ActionDiff $ view actionDiffL ge)

  atomically
    $ writeTChan writeChannel
    $ encode
    $ GameUpdate
    $ PublicGame gameId arkhamGameName (gameLogToLogEntries $ oldLog <> GameLog updatedLog) ge

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

handleMessageLog
  :: MonadIO m => IORef [Text] -> TChan BSL.ByteString -> ClientMessage -> m ()
handleMessageLog logRef writeChannel msg = liftIO $ do
  for_ (toClientText msg) $ \txt ->
    atomicModifyIORef' logRef (\logs -> (logs <> [txt], ()))
  atomically $ writeTChan writeChannel (encode $ toGameMessage msg)
 where
  toGameMessage = \case
    ClientText txt -> GameMessage txt
    ClientCard t v -> GameCard t v
    ClientTarot v -> GameTarot v
  toClientText = \case
    ClientText txt -> Just txt
    ClientCard {} -> Nothing
    ClientTarot {} -> Nothing

-- TODO: Make this a websocket message
putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  response <- requireCheckJsonBody
  writeChannel <- getChannel gameId
  updateGame (Raw $ gameMessage response) gameId userId writeChannel

deleteApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
deleteApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ delete $ do
    games <- from $ table @ArkhamGame
    where_ $ games.id ==. val gameId
    where_ $ exists $ do
      players <- from $ table @ArkhamPlayer
      where_ $ players.arkhamGameId ==. games.id
      where_ $ players.userId ==. val userId

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

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
import Arkham.Campaign.Types (CampaignAttrs)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as TheDreamEaters
import Arkham.ClassSymbol
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Difficulty
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Game.State
import Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator (lookupInvestigator)
import Arkham.Investigator.Types (Investigator)
import Arkham.Message
import Arkham.Name
import Arkham.Queue
import Conduit
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Control.Monad.Random.Class (getRandom)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce
import Data.Map.Strict qualified as Map
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text qualified as T
import Data.These
import Data.Time.Clock
import Data.UUID (nil)
import Database.Esqueleto.Experimental hiding (update, (=.))
import Database.Redis (publish, runRedis)
import Entity.Answer
import Entity.Arkham.GameRaw
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
  writeChannel <- lift $ (.channel) <$> getRoom gameId
  roomsRef <- getsYesod appGameRooms
  atomicModifyIORef' roomsRef
    $ \rooms -> (Map.adjust (\room -> room {socketClients = room.clients + 1}) gameId rooms, ())
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
        Right answer ->
          updateGame answer gameId userId writeChannel `catch` \(e :: SomeException) ->
            liftIO $ atomically $ writeTChan writeChannel $ encode $ GameError $ tshow e

  closeConnection _ = do
    roomsRef <- getsYesod appGameRooms
    clientCount <-
      atomicModifyIORef' roomsRef $ \rooms ->
        ( Map.adjust (\room -> room {socketClients = max 0 (room.clients - 1)}) gameId rooms
        , maybe 0 (\room -> max 0 (room.clients - 1)) $ Map.lookup gameId rooms
        )
    when (clientCount == 0) do
      lift $ removeChannel (gameChannel gameId)
      atomicModifyIORef' roomsRef $ \rooms -> (Map.delete gameId rooms, ())

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ tshow (e :: ConnectionException)

data GetGameJson = GetGameJson
  { playerId :: Maybe PlayerId
  , multiplayerMode :: MultiplayerVariant
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

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

data InvestigatorDetails = InvestigatorDetails
  { id :: InvestigatorId
  , classSymbol :: ClassSymbol
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data ScenarioDetails = ScenarioDetails
  { id :: ScenarioId
  , difficulty :: Difficulty
  , name :: Name
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data CampaignDetails = CampaignDetails
  { id :: CampaignId
  , difficulty :: Difficulty
  , currentCampaignMode :: Maybe TheDreamEaters.CampaignPart
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GameDetails = GameDetails
  { id :: ArkhamGameId
  , scenario :: Maybe ScenarioDetails
  , campaign :: Maybe CampaignDetails
  , gameState :: GameState
  , name :: Text
  , investigators :: [InvestigatorDetails]
  , otherInvestigators :: [InvestigatorDetails]
  , multiplayerVariant :: MultiplayerVariant
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GameDetailsEntry = FailedGameDetails Text | SuccessGameDetails GameDetails
  deriving stock (Show, Generic)

instance ToJSON GameDetailsEntry where
  toJSON = \case
    FailedGameDetails t -> object ["error" .= t]
    SuccessGameDetails gd -> toJSON gd

getApiV1ArkhamGamesR :: Handler [GameDetailsEntry]
getApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  games <- runDB $ select $ do
    (players :& games) <-
      distinct
        $ from
        $ table @ArkhamPlayer
        `innerJoin` table @ArkhamGameRaw
          `on` (\(players :& games) -> coerce players.arkhamGameId ==. games.id)
    where_ $ players.userId ==. val userId
    orderBy [desc $ games.updatedAt]
    pure games

  let
    campaignOtherInvestigators j = case parse (withObject "" (.: "otherCampaignAttrs")) j of
      Error _ -> mempty
      Success (attrs :: CampaignAttrs) -> map (\iid -> lookupInvestigator iid (PlayerId nil)) $ Map.keys attrs.decks

  pure $ flip map games \(Entity gameId game) -> do
    case fromJSON @Game (arkhamGameRawCurrentData game) of
      Success a ->
        SuccessGameDetails
          $ GameDetails
            { id = coerce gameId
            , scenario = case a.gameMode of
                This _ -> Nothing
                That s -> Just $ ScenarioDetails s.id s.difficulty s.name
                These _ s -> Just $ ScenarioDetails s.id s.difficulty s.name
            , campaign = case a.gameMode of
                This c -> Just $ CampaignDetails c.id c.difficulty c.currentCampaignMode
                That _ -> Nothing
                These c _ -> Just $ CampaignDetails c.id c.difficulty c.currentCampaignMode
            , gameState = a.gameGameState
            , name = arkhamGameRawName game
            , investigators =
                map (\(i :: Investigator) -> InvestigatorDetails i.id i.classSymbol)
                  $ toList a.gameEntities.investigators
            , otherInvestigators =
                let
                  ins = case a.gameMode of
                    This c -> campaignOtherInvestigators (toJSON c.meta)
                    That _ -> mempty
                    These c _ -> campaignOtherInvestigators (toJSON c.meta)
                 in
                  map (\i -> InvestigatorDetails i.id i.classSymbol) ins
            , multiplayerVariant = arkhamGameRawMultiplayerVariant game
            }
      Error e -> FailedGameDetails ("Failed to load " <> tshow gameId <> ": " <> T.pack e)

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
  deriving anyclass FromJSON

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
  runDB $ do
    gameId <- insert ag
    pids <- replicateM repeatCount $ insert $ ArkhamPlayer userId gameId "00000"
    gameRef <- liftIO $ newIORef game

    runGameApp (GameApp gameRef queueRef genRef (pure . const ())) $ do
      for_ pids $ \pid -> addPlayer (PlayerId $ coerce pid)
      runMessages Nothing

    updatedQueue <- liftIO $ readIORef (queueToRef queueRef)
    updatedGame <- liftIO $ readIORef gameRef

    let ag' = ag {arkhamGameCurrentData = updatedGame}

    replace gameId ag'
    insert_ $ ArkhamStep gameId (Choice mempty updatedQueue) 0 (ActionDiff [])
    pure $ toPublicGame (Entity gameId ag') mempty

putApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  response <- requireCheckJsonBody
  writeChannel <- (.channel) <$> getRoom gameId
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

  logRef <- newIORef []
  answerMessages <- handleAnswer gameJson playerId response
  let
    messages =
      [SetActivePlayer playerId | activePlayer /= playerId]
        <> answerMessages
        <> [SetActivePlayer activePlayer | activePlayer /= playerId]
  gameRef <- newIORef gameJson
  queueRef <- newQueue ((ClearUI : messages) <> currentQueue)
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
    void $ select do
      game <- from $ table @ArkhamGame
      where_ $ game.id ==. val gameId
      locking ForUpdate
      pure ()
    deleteWhere [ArkhamStepArkhamGameId P.==. gameId, ArkhamStepStep P.>. arkhamGameStep]
    replace gameId
      $ ArkhamGame
        arkhamGameName
        ge
        (arkhamGameStep + 1)
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now
    insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog
    void
      $ upsertBy
        (UniqueStep gameId (arkhamGameStep + 1))
        ( ArkhamStep
            gameId
            (Choice diffDown updatedQueue)
            (arkhamGameStep + 1)
            (ActionDiff $ view actionDiffL ge)
        )
        [ ArkhamStepChoice =. Choice diffDown updatedQueue
        , ArkhamStepActionDiff =. ActionDiff (view actionDiffL ge)
        ]

  publishToRoom gameId
    $ GameUpdate
    $ PublicGame gameId arkhamGameName (gameLogToLogEntries $ oldLog <> GameLog updatedLog) ge

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

handleMessageLog
  :: MonadIO m => IORef [Text] -> TChan BSL.ByteString -> ClientMessage -> m ()
handleMessageLog logRef writeChannel msg = liftIO $ do
  for_ (toClientText msg) $ \txt ->
    atomicModifyIORef' logRef (\logs -> (logs <> [txt], ()))
  atomically $ writeTChan writeChannel (encode $ toGameMessage msg)
 where
  toGameMessage = \case
    ClientText txt -> GameMessage txt
    ClientError txt -> GameError txt
    ClientCard t v -> GameCard t v
    ClientCardOnly i t v -> GameCardOnly i t v
    ClientTarot v -> GameTarot v
  toClientText = \case
    ClientText txt -> Just txt
    ClientError {} -> Nothing
    ClientCard {} -> Nothing
    ClientCardOnly {} -> Nothing
    ClientTarot {} -> Nothing

-- TODO: Make this a websocket message
putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  response <- requireCheckJsonBody @_ @RawGameJsonPut
  writeChannel <- (.channel) <$> getRoom gameId
  updateGame (Raw response.gameMessage) gameId userId writeChannel

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

publishToRoom :: (MonadIO m, ToJSON a, HasApp m) => ArkhamGameId -> a -> m ()
publishToRoom gameId a = do
  broker <- getsApp appMessageBroker
  case broker of
    RedisBroker redisConn _ ->
      void
        $ liftIO
        $ runRedis redisConn
        $ publish (gameChannel gameId)
        $ toStrictByteString
        $ encode a
    WebSocketBroker -> do
      writeChannel <- (.channel) <$> getRoom gameId
      atomically $ writeTChan writeChannel $ encode a

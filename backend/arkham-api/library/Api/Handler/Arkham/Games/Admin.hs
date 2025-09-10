{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Api.Handler.Arkham.Games.Admin (
  getApiV1AdminR,
  getApiV1AdminGameR,
  putApiV1AdminGameR,
  getApiV1AdminGamesR,
  putApiV1AdminGameRawR,
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
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.), (>=.))
import Import qualified as P
import Json
import Network.WebSockets (ConnectionException)
import UnliftIO.Exception hiding (Handler)
import Yesod.WebSockets

gameStream :: Maybe UserId -> ArkhamGameId -> WebSocketsT Handler ()
gameStream mUserId gameId = catchingConnectionException do
  writeChannel <- lift $ (.channel) <$> getRoom gameId
  roomsRef <- getsYesod appGameRooms
  atomicModifyIORef' roomsRef \rooms -> do
    (Map.adjust (\room -> room {socketClients = room.clients + 1}) gameId rooms, ())
  bracket (atomically $ dupTChan writeChannel) closeConnection \readChannel -> do
    race_
      (forever $ atomically (readTChan readChannel) >>= sendTextData)
      (runConduit $ sourceWS .| mapM_C (handleData writeChannel))
 where
  handleData writeChannel dataPacket = lift do
    for_ mUserId \userId ->
      case eitherDecodeStrict dataPacket of
        Left err -> $(logWarn) $ tshow err
        Right answer ->
          updateGame answer gameId userId writeChannel `catch` \(e :: SomeException) -> do
            liftIO $ atomically $ writeTChan writeChannel $ encode $ GameError $ tshow e

  closeConnection _ = do
    roomsRef <- getsYesod appGameRooms
    clientCount <- atomicModifyIORef' roomsRef \rooms ->
      ( Map.adjust (\room -> room {socketClients = max 0 (room.clients - 1)}) gameId rooms
      , maybe 0 (\room -> max 0 (room.clients - 1)) $ Map.lookup gameId rooms
      )
    when (clientCount == 0) do
      lift $ removeChannel (gameChannel gameId)
      atomicModifyIORef' roomsRef \rooms -> (Map.delete gameId rooms, ())

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

getAdminUser :: Handler (Entity User)
getAdminUser = do
  userId <- getRequestUserId
  user <- runDB $ get404 userId
  unless user.admin $ permissionDenied "You must be an admin to access this endpoint"
  pure $ Entity userId user

data AdminData = AdminData
  { currentUsers :: Int
  , activeUsers :: Int
  , activeGames :: [GameDetailsEntry]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

selectCount :: SqlQuery a -> DB Int
selectCount inner = fmap (sum . map unValue . toList) . selectOne $ inner $> countRows

getApiV1AdminR :: Handler AdminData
getApiV1AdminR = do
  _user <- getAdminUser
  recent <- addUTCTime (negate (14 * nominalDay)) <$> liftIO getCurrentTime
  runDB do
    currentUsers <- selectCount $ from $ table @User
    activeUsers <-
      fmap (sum . map unValue . toList) . selectOne $ do
        (users :& _players :& games) <-
          from
            $ table @User
            `innerJoin` table @ArkhamPlayer
              `on` (\(users :& players) -> users.id ==. players.userId)
            `innerJoin` table @ArkhamGame
              `on` (\(_ :& players :& games) -> players.arkhamGameId ==. games.id)
        where_ (games.updatedAt >=. val recent)
        pure (countDistinct users.id)

    games <- recentGames 20

    pure $ AdminData currentUsers activeUsers games

getApiV1AdminGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1AdminGameR gameId = do
  _user <- getAdminUser
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

recentGames :: Int64 -> DB [GameDetailsEntry]
recentGames n = do
  games <- select do
    games <- from $ table @ArkhamGameRaw
    orderBy [desc games.updatedAt]
    limit n
    pure games

  let
    campaignOtherInvestigators j = case parse (withObject "" (.: "otherCampaignAttrs")) j of
      Error _ -> mempty
      Success (attrs :: CampaignAttrs) -> map (`lookupInvestigator` PlayerId nil) $ Map.keys attrs.decks

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

getApiV1AdminGamesR :: Handler [GameDetailsEntry]
getApiV1AdminGamesR = do
  _user <- getAdminUser
  runDB $ recentGames 20

putApiV1AdminGameR :: ArkhamGameId -> Handler ()
putApiV1AdminGameR gameId = do
  userId <- getRequestUserId
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
  handleAnswer gameJson playerId response >>= \case
    Unhandled _ -> pure ()
    Handled answerMessages -> do
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
          locking forUpdate
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
    ClientShowDiscard v -> GameShowDiscard v
    ClientShowUnder v -> GameShowUnder v
  toClientText = \case
    ClientText txt -> Just txt
    ClientError {} -> Nothing
    ClientCard {} -> Nothing
    ClientCardOnly {} -> Nothing
    ClientTarot {} -> Nothing
    ClientShowDiscard {} -> Nothing
    ClientShowUnder {} -> Nothing

-- TODO: Make this a websocket message
putApiV1AdminGameRawR :: ArkhamGameId -> Handler ()
putApiV1AdminGameRawR gameId = do
  Entity userId _ <- getAdminUser
  response <- requireCheckJsonBody @_ @RawGameJsonPut
  writeChannel <- (.channel) <$> getRoom gameId
  updateGame (Raw response.gameMessage) gameId userId writeChannel

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

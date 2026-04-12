{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games.Shared where

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
import Control.Concurrent.MVar
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text qualified as T
import Data.These
import Data.Time.Clock
import Data.UUID (nil)
import Database.Esqueleto.Experimental hiding (update, (=.))
import Database.Redis (msgMessage, pubSub, publish, runRedis, subscribe)
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.), (>=.))
import Import qualified as P
import Json
import Network.WebSockets (ConnectionException)
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception hiding (Handler)
import Yesod.WebSockets

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream gameId = catchingConnectionException do
  writeChannel :: TChan BSL.ByteString <- lift $ (.channel) <$> getRoom gameId
  broker <- lift $ getsYesod appMessageBroker
  roomsVar <- getsYesod appGameRooms
  liftIO
    $ modifyMVar_ roomsVar
    $ pure
    . Map.adjust (\room -> room {socketClients = room.clients + 1}) gameId

  bracket (atomically $ dupTChan writeChannel) closeConnection \readChannel -> do
    mtid <- case broker of
      RedisBroker redisConn _ -> do
        tid <- liftIO
          $ async
          $ runRedis redisConn
          $ pubSub (subscribe [gameChannel gameId])
          $ \msg -> do
            atomically $ writeTChan readChannel (BSL.fromStrict $ msgMessage msg)
            pure mempty
        pure $ Just tid
      WebSocketBroker -> pure Nothing

    let stopSub = maybe (pure ()) (liftIO . cancel) mtid

    finally
      ( race_
          (forever $ atomically (readTChan readChannel) >>= sendTextData)
          (runConduit $ sourceWS .| mapM_C (handleData writeChannel))
      )
      stopSub
 where
  handleData writeChannel dataPacket = lift do
    case eitherDecodeStrict dataPacket of
      Left err -> $(logWarn) $ tshow err
      Right answer ->
        updateGame answer gameId writeChannel `catch` \(e :: SomeException) -> do
          atomically $ writeTChan writeChannel $ encode $ GameError $ tshow e

  closeConnection _ = do
    roomsVar <- getsYesod appGameRooms
    remove <- liftIO $ modifyMVar roomsVar \rooms -> pure do
      case Map.lookup gameId rooms of
        Nothing -> (rooms, False)
        Just room -> do
          let room' = room {socketClients = max 0 (room.clients - 1)}
          if room'.clients == 0
            then (Map.delete gameId rooms, True)
            else (Map.insert gameId room' rooms, False)

    when remove $ lift $ removeChannel (gameChannel gameId)

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

-- | Regular game update. The lock is held only during two short DB phases;
-- the expensive runMessages processing runs outside any transaction, so undo
-- requests are no longer blocked for the full duration of message processing.
--
-- Phase 1 (short lock): fetch state + handleAnswer (deck answers write here)
-- Phase 2 (no lock):    runMessages — all game logic runs here
-- Phase 3 (short lock): write results
updateGame :: Answer -> ArkhamGameId -> TChan BSL.ByteString -> Handler ()
updateGame response gameId writeChannel = do
  tracer <- getTracer
  oldLog <- runDB $ getGameLog gameId Nothing

  -- Phase 1: short lock — fetch state and dispatch the answer.
  -- Some answer types (DeckAnswer, DeckListAnswer) write to DB here.
  mWork <- runDB do
    lockGame gameId
    rawGame <- get404 (ArkhamGameRawKey gameId)
    let n = arkhamGameRawStep rawGame
    mLastStep <- getBy $ UniqueStep gameId n
    gameJson <- case fromJSON @Game rawGame.currentData of
      Error e -> error $ "Failed to parse game: " <> tshow e
      Success g -> pure g
    let
      Game {..} = gameJson
      currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep
    activePlayer <- runReaderT getActivePlayer gameJson
    let playerId = fromMaybe activePlayer (answerPlayer response)
    handleAnswer gameJson playerId response >>= \case
      Unhandled _ -> pure $ Left (gameJson, rawGame.name)
      Handled answerMessages -> do
        let
          messages =
            [SetActivePlayer playerId | activePlayer /= playerId]
              <> answerMessages
              <> [SetActivePlayer activePlayer | activePlayer /= playerId]
        pure $ Right (rawGame, gameJson, gameSeed, n, messages, currentQueue)

  case mWork of
    Left (gameJson, gameName) ->
      publishToRoom gameId
        $ GameUpdate
        $ PublicGame gameId gameName (gameLogToLogEntries oldLog) gameJson
    Right (rawGame, gameJson, gameSeed, n, messages, currentQueue) -> do
      -- Phase 2: no lock held — run all game logic in memory.
      -- This is the expensive part that was previously holding the lock,
      -- causing undo requests to block for 10-20 seconds.
      logRef <- newIORef []
      gameRef <- newIORef gameJson
      queueRef <- newQueue ((ClearUI : messages) <> currentQueue)
      genRef <- newIORef $ mkStdGen gameSeed
      runGameApp (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel) tracer) do
        runMessages (gameIdToText gameId) Nothing
      ge <- readIORef gameRef
      -- toJSON once, reused for both diff computation and DB store
      let ge_json = toJSON ge
          diffDown = diffValues ge_json rawGame.currentData
      updatedQueue <- readIORef $ queueToRef queueRef
      updatedLog <- readIORef logRef
      now <- liftIO getCurrentTime

      -- Phase 3: short lock — write results.
      runDB do
        deleteWhere [ArkhamStepArkhamGameId P.==. gameId, ArkhamStepStep P.>. n]
        replace (ArkhamGameRawKey gameId) $
          ArkhamGameRaw
            rawGame.name
            ge_json
            (n + 1)
            rawGame.multiplayerVariant
            rawGame.createdAt
            now
        insertMany_ $ map (newLogEntry gameId n now) updatedLog
        void
          $ upsertBy
            (UniqueStep gameId (n + 1))
            ( ArkhamStep gameId
                (Choice diffDown updatedQueue)
                (n + 1)
                (ActionDiff $ view actionDiffL ge)
            )
            [ ArkhamStepChoice =. Choice diffDown updatedQueue
            , ArkhamStepActionDiff =. ActionDiff (view actionDiffL ge)
            ]

      publishToRoom gameId
        $ GameUpdate
        $ PublicGame
          gameId
          rawGame.name
          (gameLogToLogEntries $ oldLog <> GameLog updatedLog)
          ge

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
    ClientUI txt -> GameUI txt
    ClientCard t v -> GameCard t v
    ClientCardOnly i t v -> GameCardOnly i t v
    ClientTarot v -> GameTarot v
    ClientShowDiscard v -> GameShowDiscard v
    ClientShowUnder v -> GameShowUnder v
    ClientPlayabilityReport cid cc chks -> GamePlayabilityInfo cid cc chks
  toClientText = \case
    ClientText txt -> Just txt
    ClientError {} -> Nothing
    ClientUI {} -> Nothing
    ClientCard {} -> Nothing
    ClientCardOnly {} -> Nothing
    ClientTarot {} -> Nothing
    ClientShowDiscard {} -> Nothing
    ClientShowUnder {} -> Nothing
    ClientPlayabilityReport {} -> Nothing

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

toGameDetailsEntry :: Entity ArkhamGameRaw -> GameDetailsEntry
toGameDetailsEntry (Entity gameId game) =
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
 where
  campaignOtherInvestigators j = case parse (withObject "" (.: "otherCampaignAttrs")) j of
    Error _ -> mempty
    Success (attrs :: CampaignAttrs) -> map (`lookupInvestigator` PlayerId nil) $ Map.keys attrs.decks

deleteRoom :: ArkhamGameId -> Handler ()
deleteRoom gameId = do
  roomsVar <- getsYesod appGameRooms
  liftIO $ modifyMVar_ roomsVar $ pure . Map.delete gameId

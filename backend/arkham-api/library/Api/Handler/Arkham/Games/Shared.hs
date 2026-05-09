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
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Lens (view)
import Data.IntMap.Strict qualified as IntMap
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
  room <- lift $ getRoom gameId
  broker <- lift $ getsYesod appMessageBroker
  let broadcast = broadcastToRoom room

  let cleanup subId = do
        unsubscribeFromRoom room subId
        lift $ decrRoomMember gameId
        -- If this was the last subscriber, drop the room from the map so
        -- it doesn't accumulate orphaned entries.
        isEmpty <- liftIO $ atomically do
          IntMap.null <$> readTVar (roomSubscribers room)
        when isEmpty do
          roomsVar <- lift $ getsYesod appGameRooms
          liftIO $ modifyMVar_ roomsVar $ pure . Map.delete gameId
          lift $ removeChannel (gameChannel gameId)

  let acquire = do
        s <- subscribeToRoom room
        lift $ incrRoomMember gameId
        pure s

  bracket acquire (\(subId, _) -> cleanup subId) \(_subId, sub) -> do
    let Subscriber {subQueue, subOverflow} = sub
    mtid <- case broker of
      RedisBroker redisConn _ -> do
        tid <- liftIO
          $ async
          $ runRedis redisConn
          $ pubSub (subscribe [gameChannel gameId])
          $ \msg -> do
            broadcast (BSL.fromStrict $ msgMessage msg)
            pure mempty
        pure $ Just tid
      WebSocketBroker -> pure Nothing

    let stopSub = maybe (pure ()) (liftIO . cancel) mtid

    let sender =
          forever
            ( do
                msg <- atomically do
                  overflowed <- readTVar subOverflow
                  if overflowed
                    then throwSTM SlowSubscriber
                    else readTBQueue subQueue
                sendTextData msg
            )
            `catch` (\(_ :: SlowSubscriber) -> pure ())

    finally
      ( race_
          sender
          (runConduit $ sourceWS .| mapM_C (handleData room broadcast))
      )
      stopSub
 where
  handleData room broadcast dataPacket = lift do
    case eitherDecodeStrict dataPacket of
      Left err -> $(logWarn) $ tshow err
      Right answer ->
        updateGame answer gameId (Just room) `catch` \(e :: SomeException) -> do
          liftIO $ broadcast $ encode $ GameError $ tshow e

data SlowSubscriber = SlowSubscriber
  deriving stock Show
  deriving anyclass Exception

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
  , hasOpenSeats :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data GameDetailsEntry = FailedGameDetails Text | SuccessGameDetails GameDetails
  deriving stock (Show, Generic)

instance ToJSON GameDetailsEntry where
  toJSON = \case
    FailedGameDetails t -> object ["error" .= t]
    SuccessGameDetails gd -> toJSON gd

-- | A broadcast callback. Used to fan out log lines and game-state updates
-- to every WebSocket subscriber on a room. May be a no-op if there are no
-- subscribers (e.g. a direct REST PUT with no client listening), in which
-- case messages are silently dropped instead of buffered indefinitely.
type Broadcast = BSL.ByteString -> IO ()

updateGame :: Answer -> ArkhamGameId -> Maybe Room -> Handler ()
updateGame response gameId mRoom = do
  let broadcast :: Broadcast
      broadcast = case mRoom of
        Nothing -> \_ -> pure ()
        Just room -> broadcastToRoom room
  tracer <- getTracer
  oldLog <- runDB $ getGameLog gameId Nothing
  (ArkhamGame {..}, updatedLog) <- runDB $ atomicallyWithGame gameId \g@ArkhamGame {..} -> do
    mLastStep <- getBy $ UniqueStep gameId arkhamGameStep
    let
      gameJson@Game {..} = arkhamGameCurrentData
      currentQueue =
        maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

    activePlayer <- runReaderT getActivePlayer gameJson

    let playerId = fromMaybe activePlayer (answerPlayer response)

    logRef <- newIORef []
    handleAnswer gameJson playerId response >>= \case
      Unhandled _ -> pure (g, [])
      Handled answerMessages -> do
        let
          messages =
            [SetActivePlayer playerId | activePlayer /= playerId]
              <> answerMessages
              <> [SetActivePlayer activePlayer | activePlayer /= playerId]
        gameRef <- newIORef gameJson
        queueRef <- newQueue ((ClearUI : messages) <> currentQueue)
        genRef <- newIORef $ mkStdGen gameSeed

        runGameApp (GameApp gameRef queueRef genRef (handleMessageLog logRef broadcast) tracer) do
          runMessages (gameIdToText gameId) Nothing

        ge <- readIORef gameRef
        let diffDown = diff ge arkhamGameCurrentData

        updatedQueue <- readIORef $ queueToRef queueRef
        updatedLog <- readIORef logRef

        now <- liftIO getCurrentTime
        deleteWhere [ArkhamStepArkhamGameId P.==. gameId, ArkhamStepStep P.>. arkhamGameStep]
        let g' =
              ArkhamGame
                arkhamGameName
                ge
                (arkhamGameStep + 1)
                arkhamGameMultiplayerVariant
                arkhamGameCreatedAt
                now
        replace gameId g'
        insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog
        void
          $ upsertBy
            (UniqueStep gameId (arkhamGameStep + 1))
            ( ArkhamStep gameId
                (Choice diffDown updatedQueue)
                (arkhamGameStep + 1)
                (ActionDiff $ view actionDiffL ge)
            )
            [ ArkhamStepChoice =. Choice diffDown updatedQueue
            , ArkhamStepActionDiff =. ActionDiff (view actionDiffL ge)
            ]
        pure (g', updatedLog)

  publishToRoom gameId
    $ GameUpdate
    $ PublicGame
      gameId
      arkhamGameName
      (gameLogToLogEntries $ oldLog <> GameLog updatedLog)
      arkhamGameCurrentData

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

handleMessageLog
  :: MonadIO m => IORef [Text] -> Broadcast -> ClientMessage -> m ()
handleMessageLog logRef broadcast msg = liftIO $ do
  for_ (toClientText msg) $ \txt ->
    atomicModifyIORef' logRef (\logs -> (logs <> [txt], ()))
  broadcast (encode $ toGameMessage msg)
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
    WebSocketBroker ->
      -- Don't create a Room here. If nobody is subscribed, drop the
      -- update on the floor; the next subscriber will read the latest
      -- state from the database when they connect.
      lookupRoom gameId >>= traverse_ (`broadcastToRoom` encode a)

toGameDetailsEntry :: Entity ArkhamGameRaw -> Int -> GameDetailsEntry
toGameDetailsEntry (Entity gameId game) playerCount =
  case fromJSON @Game (arkhamGameRawCurrentData game) of
    Success a ->
      let
        investigators =
          map (\(i :: Investigator) -> InvestigatorDetails i.id i.classSymbol)
            $ toList a.gameEntities.investigators
        variant = arkhamGameRawMultiplayerVariant game
       in
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
            , investigators
            , otherInvestigators =
                let
                  ins = case a.gameMode of
                    This c -> campaignOtherInvestigators (toJSON c.meta)
                    That _ -> mempty
                    These c _ -> campaignOtherInvestigators (toJSON c.meta)
                 in
                  map (\i -> InvestigatorDetails i.id i.classSymbol) ins
            , multiplayerVariant = variant
            , hasOpenSeats = variant == WithFriends && playerCount < length investigators
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

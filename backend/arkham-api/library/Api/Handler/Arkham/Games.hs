{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games (
  getApiV1ArkhamGameR,
  getApiV1ArkhamGameExportR,
  getApiV1ArkhamGameSpectateR,
  getApiV1ArkhamGamesR,
  postApiV1ArkhamGamesR,
  postApiV1ArkhamGamesImportR,
  putApiV1ArkhamGameR,
  deleteApiV1ArkhamGameR,
  putApiV1ArkhamGameRawR,
) where

import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Classes.HasQueue
import Arkham.Decklist
import Arkham.Difficulty
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Id
import Arkham.Message
import Conduit
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Control.Monad.Random.Class (getRandom)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable (for)
import Database.Esqueleto.Experimental hiding (update)
import Entity.Answer
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Json
import Network.WebSockets (ConnectionException)
import Safe (fromJustNote)
import UnliftIO.Exception hiding (Handler)
import Yesod.WebSockets

gameStream :: Maybe UserId -> ArkhamGameId -> WebSocketsT Handler ()
gameStream mUserId gameId = catchingConnectionException $ do
  writeChannel <- lift $ getChannel gameId
  gameChannelClients <- appGameChannelClients <$> getYesod
  atomicModifyIORef' gameChannelClients $
    \channelClients -> (Map.insertWith (+) gameId 1 channelClients, ())
  bracket (atomically $ dupTChan writeChannel) closeConnection $
    \readChannel ->
      race_
        (forever $ atomically (readTChan readChannel) >>= sendTextData)
        (runConduit $ sourceWS .| mapM_C (handleData writeChannel))
 where
  handleData writeChannel dataPacket = lift $ do
    for_ mUserId $ \userId ->
      for_ (decode dataPacket) $ \answer ->
        updateGame answer gameId userId writeChannel

  closeConnection _ = do
    gameChannelsRef <- appGameChannels <$> lift getYesod
    gameChannelClientsRef <- appGameChannelClients <$> lift getYesod
    clientCount <-
      atomicModifyIORef' gameChannelClientsRef $ \channelClients ->
        ( Map.adjust pred gameId channelClients
        , Map.findWithDefault 1 gameId channelClients - 1
        )
    when (clientCount == 0) $
      atomicModifyIORef' gameChannelsRef $
        \gameChannels' -> (Map.delete gameId gameChannels', ())

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ tshow (e :: ConnectionException)

data GetGameJson = GetGameJson
  { investigatorId :: Maybe InvestigatorId
  , multiplayerMode :: MultiplayerVariant
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

getApiV1ArkhamGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  webSockets (gameStream (Just userId) gameId)
  ge <- runDB $ get404 gameId
  ArkhamPlayer {..} <- runDB $ entityVal <$> getBy404 (UniquePlayer userId gameId)
  let
    Game {..} = arkhamGameCurrentData ge
    investigatorId = case arkhamGameMultiplayerVariant ge of
      Solo -> coerce gameActiveInvestigatorId
      WithFriends -> coerce arkhamPlayerInvestigatorId
  gameLog <- runDB $ getGameLog gameId Nothing
  pure $
    GetGameJson
      (Just investigatorId)
      (arkhamGameMultiplayerVariant ge)
      (PublicGame gameId (arkhamGameName ge) (gameLogToLogEntries gameLog) (arkhamGameCurrentData ge))

getApiV1ArkhamGameSpectateR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameSpectateR gameId = do
  webSockets $ gameStream Nothing gameId
  ge <- runDB $ get404 gameId
  let
    Game {..} = arkhamGameCurrentData ge
    investigatorId = coerce gameActiveInvestigatorId
  gameLog <- runDB $ getGameLog gameId Nothing
  pure $
    GetGameJson
      (Just investigatorId)
      (arkhamGameMultiplayerVariant ge)
      (PublicGame gameId (arkhamGameName ge) (gameLogToLogEntries gameLog) (arkhamGameCurrentData ge))

getApiV1ArkhamGamesR :: Handler [PublicGame ArkhamGameId]
getApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  games <- runDB $ select $ do
    (players :& games) <-
      from
        $ table @ArkhamPlayer
          `InnerJoin` table @ArkhamGame
        `on` ( \(players :& games) ->
                players ^. ArkhamPlayerArkhamGameId ==. games ^. persistIdField
             )
    where_ (players ^. ArkhamPlayerUserId ==. val userId)
    orderBy [desc $ games ^. ArkhamGameUpdatedAt]
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

getApiV1ArkhamGameExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameExportR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ do
    ge <- get404 gameId
    players <- select $ do
      players <- from $ table @ArkhamPlayer
      where_ (players ^. ArkhamPlayerArkhamGameId ==. val gameId)
      pure players
    steps <- select $ do
      steps <- from $ table @ArkhamStep
      where_ $ steps ^. ArkhamStepArkhamGameId ==. val gameId
      orderBy [desc $ steps ^. ArkhamStepStep]
      pure steps

    entries <- getGameLogEntries gameId

    pure $
      ArkhamExport
        { aeCampaignPlayers = map (arkhamPlayerInvestigatorId . entityVal) players
        , aeCampaignData = arkhamGameToExportData ge (map entityVal steps) entries
        }

postApiV1ArkhamGamesImportR :: Handler (PublicGame ArkhamGameId)
postApiV1ArkhamGamesImportR = do
  -- Convert to multiplayer solitaire
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  eExportData :: Either String ArkhamExport <-
    fmap eitherDecodeStrict'
      . fileSourceByteString
      . snd
      . fromJustNote "No export file uploaded"
      . headMay
      . snd
      =<< runRequestBody
  now <- liftIO getCurrentTime

  case eExportData of
    Left err -> error $ T.pack err
    Right export -> do
      let
        ArkhamGameExportData {..} = aeCampaignData export
        investigatorIds = aeCampaignPlayers export
      key <- runDB $ do
        gameId <-
          insert $
            ArkhamGame agedName agedCurrentData agedStep Solo now now
        insertMany_ $ map (\e -> e {arkhamLogEntryArkhamGameId = gameId}) agedLog
        traverse_ (insert_ . ArkhamPlayer userId gameId) investigatorIds
        traverse_
          ( \s ->
              insert_ $
                ArkhamStep gameId (arkhamStepChoice s) (arkhamStepStep s) (arkhamStepActionDiff s)
          )
          agedSteps
        pure gameId
      pure $
        toPublicGame
          (Entity key $ ArkhamGame agedName agedCurrentData agedStep Solo now now)
          (GameLog $ map arkhamLogEntryBody agedLog)

postApiV1ArkhamGamesR :: Handler (PublicGame ArkhamGameId)
postApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  CreateGamePost {..} <- requireCheckJsonBody
  decks' <- for (catMaybes deckIds) $ \deckId -> do
    deck <- runDB $ get404 deckId
    when (arkhamDeckUserId deck /= userId) notFound
    pure $ arkhamDeckList deck
  decks <- maybe (invalidArgs ["must have one deck"]) pure $ nonEmpty decks'
  let
    investigatorId = decklistInvestigatorId $ NonEmpty.head decks

  newGameSeed <- liftIO getRandom
  genRef <- newIORef (mkStdGen newGameSeed)
  now <- liftIO getCurrentTime
  case campaignId of
    Just cid -> do
      (queueRef, game) <-
        liftIO $
          newCampaign cid scenarioId newGameSeed playerCount decks difficulty
      gameRef <- newIORef game
      runGameApp
        (GameApp gameRef queueRef genRef $ pure . const ())
        (runMessages Nothing)
      ge <- readIORef gameRef
      updatedQueue <- readIORef (queueToRef queueRef)
      key <- runDB $ do
        gameId <- insert $ ArkhamGame campaignName ge 0 multiplayerVariant now now
        insert_ $ ArkhamPlayer userId gameId (coerce investigatorId)
        insert_ $ ArkhamStep gameId (Choice mempty updatedQueue) 0 (ActionDiff [])
        pure gameId
      pure $
        toPublicGame
          (Entity key $ ArkhamGame campaignName ge 0 multiplayerVariant now now)
          mempty
    Nothing -> case scenarioId of
      Just sid -> do
        (queueRef, game) <-
          liftIO $
            newScenario sid newGameSeed playerCount decks difficulty
        gameRef <- newIORef game
        runGameApp
          (GameApp gameRef queueRef genRef $ pure . const ())
          (runMessages Nothing)
        ge <- readIORef gameRef
        let diffDown = diff ge game
        updatedQueue <- readIORef (queueToRef queueRef)
        key <- runDB $ do
          gameId <- insert $ ArkhamGame campaignName ge 0 multiplayerVariant now now
          insert_ $ ArkhamPlayer userId gameId (coerce investigatorId)
          insert_ $ ArkhamStep gameId (Choice diffDown updatedQueue) 0 (ActionDiff [])
          pure gameId
        pure $
          toPublicGame
            (Entity key $ ArkhamGame campaignName ge 0 multiplayerVariant now now)
            mempty
      Nothing -> error "missing either campaign id or scenario id"

putApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameR gameId = do
  response <- requireCheckJsonBody
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  writeChannel <- getChannel gameId
  updateGame response gameId userId writeChannel

updateGame :: Answer -> ArkhamGameId -> UserId -> TChan BSL.ByteString -> Handler ()
updateGame response gameId userId writeChannel = do
  (Entity pid arkhamPlayer@ArkhamPlayer {..}, ArkhamGame {..}) <-
    runDB $
      (,) <$> getBy404 (UniquePlayer userId gameId) <*> get404 gameId
  mLastStep <- runDB $ getBy $ UniqueStep gameId arkhamGameStep
  let
    gameJson@Game {..} = arkhamGameCurrentData
    investigatorId =
      fromMaybe
        (coerce arkhamPlayerInvestigatorId)
        (answerInvestigator response)
    messages = handleAnswer gameJson investigatorId response
    currentQueue =
      maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

  gameRef <- newIORef gameJson
  queueRef <- newQueue (messages <> currentQueue)
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
    replace gameId $
      ArkhamGame
        arkhamGameName
        ge
        (arkhamGameStep + 1)
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now
    insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog
    insert_ $
      ArkhamStep
        gameId
        (Choice diffDown updatedQueue)
        (arkhamGameStep + 1)
        (ActionDiff $ view actionDiffL ge)
    when (arkhamGameMultiplayerVariant == Solo) $
      replace pid $
        arkhamPlayer
          { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
          }

  atomically $
    writeTChan writeChannel $
      encode $
        GameUpdate $
          PublicGame gameId arkhamGameName (gameLogToLogEntries $ oldLog <> GameLog updatedLog) ge

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

handleMessageLog
  :: MonadIO m => IORef [Text] -> TChan BSL.ByteString -> Text -> m ()
handleMessageLog logRef writeChannel msg = liftIO $ do
  atomicModifyIORef' logRef (\logs -> (logs <> [msg], ()))
  atomically $ writeTChan writeChannel (encode $ GameMessage msg)

putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  void $ fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  mLastStep <- runDB $ getBy $ UniqueStep gameId arkhamGameStep
  let
    gameJson@Game {..} = arkhamGameCurrentData
    message = gameMessage response
    currentQueue =
      maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep
  gameRef <- newIORef gameJson
  queueRef <- newQueue (message : currentQueue)
  logRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId
  runGameApp
    (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel))
    (runMessages Nothing)
  ge <- readIORef gameRef
  updatedQueue <- readIORef (queueToRef queueRef)
  let diffDown = diff ge arkhamGameCurrentData
  updatedLog <- readIORef logRef
  atomically $
    writeTChan
      writeChannel
      (encode $ GameUpdate $ PublicGame gameId arkhamGameName updatedLog ge)
  now <- liftIO getCurrentTime
  runDB $ do
    replace
      gameId
      ( ArkhamGame
          arkhamGameName
          ge
          (arkhamGameStep + 1)
          arkhamGameMultiplayerVariant
          arkhamGameCreatedAt
          now
      )
    insertMany_ $ map (newLogEntry gameId arkhamGameStep now) updatedLog
    insert_ $
      ArkhamStep
        gameId
        (Choice diffDown updatedQueue)
        (arkhamGameStep + 1)
        (ActionDiff $ view actionDiffL ge)

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

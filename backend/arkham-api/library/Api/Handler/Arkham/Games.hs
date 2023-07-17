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
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
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
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable (for)
import Database.Esqueleto.Experimental hiding (update)
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, on, (==.))
import Json
import Network.WebSockets (ConnectionException)
import Safe (fromJustNote)
import UnliftIO.Exception hiding (Handler)
import Yesod.WebSockets

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream gameId = catchingConnectionException $ do
  writeChannel <- lift $ getChannel gameId
  gameChannelClients <- appGameChannelClients <$> getYesod
  atomicModifyIORef' gameChannelClients $
    \channelClients -> (Map.insertWith (+) gameId 1 channelClients, ())
  bracket (atomically $ dupTChan writeChannel) closeConnection $
    \readChannel ->
      race_
        (forever $ atomically (readTChan readChannel) >>= sendTextData)
        (runConduit $ sourceWS .| mapM_C (atomically . writeTChan writeChannel))
 where
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
  webSockets (gameStream gameId)
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  ArkhamPlayer {..} <-
    runDB $
      entityVal
        <$> getBy404
          (UniquePlayer userId gameId)
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
  webSockets $ gameStream gameId
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

data SetRecordedEntry
  = SetAsCrossedOut Json.Value
  | SetAsRecorded Json.Value
  deriving stock (Show)

data StandaloneSetting
  = SetKey CampaignLogKey Bool
  | SetRecorded CampaignLogKey SomeRecordableType [SetRecordedEntry]
  deriving stock (Show)

makeStandaloneCampaignLog :: [StandaloneSetting] -> CampaignLog
makeStandaloneCampaignLog = foldl' applySetting mkCampaignLog
 where
  applySetting :: CampaignLog -> StandaloneSetting -> CampaignLog
  applySetting cl (SetKey k True) = setCampaignLogKey k cl
  applySetting cl (SetKey k False) = deleteCampaignLogKey k cl
  applySetting cl (SetRecorded k rt vs) =
    case rt of
      (SomeRecordableType RecordableCardCode) ->
        let entries = map (toEntry @CardCode) vs
        in  setCampaignLogRecorded k entries cl
      (SomeRecordableType RecordableMemento) ->
        let entries = map (toEntry @Memento) vs
        in  setCampaignLogRecorded k entries cl
  toEntry :: forall a. (Recordable a) => SetRecordedEntry -> SomeRecorded
  toEntry (SetAsRecorded e) = case fromJSON @a e of
    Success a -> recorded a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err
  toEntry (SetAsCrossedOut e) = case fromJSON @a e of
    Success a -> crossedOut a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err

instance FromJSON StandaloneSetting where
  parseJSON = withObject "StandaloneSetting" $ \o -> do
    t <- o .: "type"
    case t of
      "ToggleKey" -> do
        k <- o .: "key"
        v <- o .: "content"
        pure $ SetKey k v
      "ToggleCrossedOut" -> do
        k <- o .: "key"
        rt <- o .: "recordable"
        v <- o .: "content"
        pure $ SetRecorded k rt v
      _ -> fail $ "No such standalone setting" <> t

instance FromJSON SetRecordedEntry where
  parseJSON = withObject "SetRecordedEntry" $ \o -> do
    k <- o .: "key"
    v <- o .: "content"
    pure $ case v of
      True -> SetAsCrossedOut k
      False -> SetAsRecorded k

-- {
--   "keys": [],
--   "counts": {
--     "PiecesOfEvidenceWereLeftBehind": 6
--   },
--   "sets": {
--     "MissingPersons": {
--       "recordable": "RecordableCardCode",
--       "entries": [
--         {
--           "tag": "CrossedOut",
--           "value": "05046"
--         },
--         {
--           "tag": "CrossedOut",
--           "value": "05047"
--         },
--         {
--           "tag": "Recorded",
--           "value": "05048"
--         },
--         {
--           "tag": "Recorded",
--           "value": "05049"
--         }
--       ]
--     }
--   }
-- }
data CampaignRecorded = CampaignRecorded
  { recordable :: SomeRecordableType
  , entries :: [CampaignRecordedEntry]
  }
  deriving stock (Show)

data CampaignRecordedEntry = CampaignEntryRecorded Json.Value | CampaignEntryCrossedOut Json.Value
  deriving stock (Show)

instance FromJSON CampaignRecordedEntry where
  parseJSON = withObject "CampaignRecordedEntry" $ \o -> do
    t :: Text <- o .: "tag"
    case t of
      "CrossedOut" -> CampaignEntryCrossedOut <$> o .: "value"
      "Recorded" -> CampaignEntryRecorded <$> o .: "value"
      _ -> fail $ "Invalid key" <> T.unpack t

data CampaignSettings = CampaignSettings
  { keys :: [CampaignLogKey]
  , counts :: Map CampaignLogKey Int
  , sets :: Map CampaignLogKey CampaignRecorded
  }
  deriving stock (Show)

instance FromJSON CampaignSettings where
  parseJSON = withObject "CampaignSettings" $ \o -> do
    keys <- o .: "keys"
    counts <- o .: "counts"
    sets <- o .: "sets"
    pure $ CampaignSettings keys counts sets

instance FromJSON CampaignRecorded where
  parseJSON = withObject "CampaignRecorded" $ \o -> do
    rt <- o .: "recordable"
    entries <- o .: "entries"
    pure $ CampaignRecorded rt entries

makeCampaignLog :: CampaignSettings -> CampaignLog
makeCampaignLog settings =
  mkCampaignLog
    { campaignLogRecorded = Set.fromList (keys settings)
    , campaignLogRecordedCounts = counts settings
    , campaignLogRecordedSets = fmap toSomeRecorded $ sets settings
    , campaignLogOrderedKeys = keys settings
    }
 where
  toSomeRecorded :: CampaignRecorded -> [SomeRecorded]
  toSomeRecorded (CampaignRecorded rt entries) =
    case rt of
      (SomeRecordableType RecordableCardCode) -> map (toEntry @CardCode) entries
      (SomeRecordableType RecordableMemento) -> map (toEntry @Memento) entries
  toEntry :: forall a. (Recordable a) => CampaignRecordedEntry -> SomeRecorded
  toEntry (CampaignEntryRecorded e) = case fromJSON @a e of
    Success a -> recorded a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err
  toEntry (CampaignEntryCrossedOut e) = case fromJSON @a e of
    Success a -> crossedOut a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err

data CreateGamePost = CreateGamePost
  { deckIds :: [Maybe ArkhamDeckId]
  , playerCount :: Int
  , campaignId :: Maybe CampaignId
  , scenarioId :: Maybe ScenarioId
  , difficulty :: Difficulty
  , campaignName :: Text
  , multiplayerVariant :: MultiplayerVariant
  , settings :: [StandaloneSetting]
  , campaignLog :: Maybe CampaignSettings
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

getApiV1ArkhamGameExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameExportR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  players <- runDB $ select $ do
    players <- from $ table @ArkhamPlayer
    where_ (players ^. ArkhamPlayerArkhamGameId ==. val gameId)
    pure players
  steps <- runDB $ select $ do
    steps <- from $ table @ArkhamStep
    where_ $ steps ^. ArkhamStepArkhamGameId ==. val gameId
    orderBy [desc $ steps ^. ArkhamStepStep]
    pure steps

  entries <- runDB $ getGameLogEntries gameId

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
                ArkhamStep gameId (arkhamStepChoice s) (arkhamStepStep s)
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
      let campaignLog' = makeCampaignLog <$> traceShowId campaignLog
      (queueRef, game) <-
        liftIO $
          newCampaign cid scenarioId newGameSeed playerCount decks difficulty campaignLog'
      gameRef <- newIORef game
      runGameApp
        (GameApp gameRef queueRef genRef $ pure . const ())
        (runMessages Nothing)
      ge <- readIORef gameRef
      updatedQueue <- readIORef (queueToRef queueRef)
      key <- runDB $ do
        gameId <-
          insert $
            ArkhamGame campaignName ge 0 multiplayerVariant now now
        insert_ $ ArkhamPlayer userId gameId (coerce investigatorId)
        insert_ $ ArkhamStep gameId (Choice mempty updatedQueue) 0
        pure gameId
      pure $
        toPublicGame
          (Entity key (ArkhamGame campaignName ge 0 multiplayerVariant now now))
          mempty
    Nothing -> case scenarioId of
      Just sid -> do
        let standaloneCampaignLog = makeStandaloneCampaignLog settings
        (queueRef, game) <-
          liftIO $
            newScenario sid newGameSeed playerCount decks difficulty (Just standaloneCampaignLog)
        gameRef <- newIORef game
        runGameApp
          (GameApp gameRef queueRef genRef $ pure . const ())
          (runMessages Nothing)
        ge <- readIORef gameRef
        let diffDown = diff ge game
        updatedQueue <- readIORef (queueToRef queueRef)
        key <- runDB $ do
          gameId <-
            insert $
              ArkhamGame campaignName ge 0 multiplayerVariant now now
          insert_ $ ArkhamPlayer userId gameId (coerce investigatorId)
          insert_ $ ArkhamStep gameId (Choice diffDown updatedQueue) 0
          pure gameId
        pure $
          toPublicGame
            (Entity key (ArkhamGame campaignName ge 0 multiplayerVariant now now))
            mempty
      Nothing -> error "missing either campaign id or scenario id"

data Answer
  = Answer QuestionResponse
  | PaymentAmountsAnswer PaymentAmountsResponse
  | AmountsAnswer AmountsResponse
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data QuestionResponse = QuestionResponse
  { qrChoice :: Int
  , qrInvestigatorId :: Maybe InvestigatorId
  }
  deriving stock (Generic)

newtype PaymentAmountsResponse = PaymentAmountsResponse
  {parAmounts :: Map InvestigatorId Int}
  deriving stock (Generic)

newtype AmountsResponse = AmountsResponse
  {arAmounts :: Map Text Int}
  deriving stock (Generic)

instance FromJSON QuestionResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "qr"

instance FromJSON PaymentAmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "par"

instance FromJSON AmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ar"

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [x | (i, x) <- zip [0 ..] xs, i /= n])

putApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  mLastStep <- runDB $ getBy $ UniqueStep gameId arkhamGameStep
  Entity pid arkhamPlayer@ArkhamPlayer {..} <-
    runDB $
      getBy404 (UniquePlayer userId gameId)
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
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId
  runGameApp
    (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel))
    (runMessages Nothing)
  ge <- readIORef gameRef
  let diffDown = diff ge arkhamGameCurrentData

  updatedQueue <- readIORef (queueToRef queueRef)
  oldLog <- runDB $ getGameLog gameId Nothing
  updatedLog <- readIORef logRef
  now <- liftIO getCurrentTime
  void $ runDB $ do
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
      ArkhamStep gameId (Choice diffDown updatedQueue) (arkhamGameStep + 1)
    case arkhamGameMultiplayerVariant of
      Solo ->
        replace pid $
          arkhamPlayer
            { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
            }
      WithFriends -> pure ()

  atomically $
    writeTChan
      writeChannel
      ( encode $
          GameUpdate $
            PublicGame gameId arkhamGameName (gameLogToLogEntries $ oldLog <> GameLog updatedLog) ge
      )

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

handleMessageLog
  :: (MonadIO m) => IORef [Text] -> TChan BSL.ByteString -> Text -> m ()
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
  void $ runDB $ do
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
    insert $
      ArkhamStep gameId (Choice diffDown updatedQueue) (arkhamGameStep + 1)

deleteApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
deleteApiV1ArkhamGameR gameId = void $ runDB $ do
  delete $ do
    entries <- from $ table @ArkhamLogEntry
    where_ $ entries.arkhamGameId ==. val gameId
  delete $ do
    steps <- from $ table @ArkhamStep
    where_ $ steps.arkhamGameId ==. val gameId
  delete $ do
    players <- from $ table @ArkhamPlayer
    where_ $ players.arkhamGameId ==. val gameId
  delete $ do
    games <- from $ table @ArkhamGame
    where_ $ games.id ==. val gameId

answerInvestigator :: Answer -> Maybe InvestigatorId
answerInvestigator = \case
  Answer response -> qrInvestigatorId response
  AmountsAnswer _ -> Nothing
  PaymentAmountsAnswer _ -> Nothing

handleAnswer :: Game -> InvestigatorId -> Answer -> [Message]
handleAnswer Game {..} investigatorId = \case
  AmountsAnswer response -> case Map.lookup investigatorId gameQuestion of
    Just (ChooseAmounts _ _ _ target) ->
      [ ResolveAmounts
          investigatorId
          (Map.toList $ arAmounts response)
          target
      ]
    Just (QuestionLabel _ (ChooseAmounts _ _ _ target)) ->
      [ ResolveAmounts
          investigatorId
          (Map.toList $ arAmounts response)
          target
      ]
    _ -> error "Wrong question type"
  PaymentAmountsAnswer response ->
    case Map.lookup investigatorId gameQuestion of
      Just (ChoosePaymentAmounts _ _ info) ->
        let
          costMap =
            Map.fromList $
              map (\(PaymentAmountChoice iid _ _ cost) -> (iid, cost)) info
        in
          concatMap
            ( \(iid, n) ->
                replicate n (Map.findWithDefault Noop iid costMap)
            )
            $ Map.toList (parAmounts response)
      _ -> error "Wrong question type"
  Answer response ->
    let
      q =
        fromJustNote
          "Invalid question type"
          (Map.lookup investigatorId gameQuestion)
    in
      go id q response
 where
  go
    :: (Question Message -> Question Message)
    -> Question Message
    -> QuestionResponse
    -> [Message]
  go f q response = case q of
    QuestionLabel lbl q' -> go (QuestionLabel lbl) q' response
    Read t qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ Read t qs]
      Just msg -> [uiToRun msg]
    ChooseOne qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ ChooseOne qs]
      Just msg -> [uiToRun msg]
    ChooseN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', msgs'') ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask investigatorId $ f $ ChooseN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseN n msgs'']
    ChooseUpToN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m'@(Done _), _) -> [uiToRun m']
        (Just m', msgs'') ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask investigatorId $ f $ ChooseUpToN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseUpToN n msgs'']
    ChooseOneAtATime msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', msgs'') ->
          [uiToRun m', Ask investigatorId $ f $ ChooseOneAtATime msgs'']
        (Nothing, msgs'') ->
          [Ask investigatorId $ f $ ChooseOneAtATime msgs'']
    ChooseSome msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just (Done _), _) -> []
        (Just m', msgs'') -> case msgs'' of
          [] -> [uiToRun m']
          [Done _] -> [uiToRun m']
          rest -> [uiToRun m', Ask investigatorId $ f $ ChooseSome rest]
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseSome msgs'']
    ChooseSome1 doneMsg msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just (Done _), _) -> []
        (Just m', msgs'') -> case msgs'' of
          [] -> [uiToRun m']
          [Done _] -> [uiToRun m']
          rest -> [uiToRun m', Ask investigatorId $ f $ ChooseSome $ Done doneMsg : rest]
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseSome $ Done doneMsg : msgs'']
    PickSupplies remaining chosen qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ PickSupplies remaining chosen qs]
      Just msg -> [uiToRun msg]
    DropDown qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ DropDown qs]
      Just (_, msg) -> [msg]
    _ -> error "Wrong question type"

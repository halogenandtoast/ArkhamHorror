{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , getApiV1ArkhamGameExportR
  , getApiV1ArkhamGameSpectateR
  , getApiV1ArkhamGamesR
  , postApiV1ArkhamGamesR
  , postApiV1ArkhamGamesImportR
  , putApiV1ArkhamGameR
  , deleteApiV1ArkhamGameR
  , putApiV1ArkhamGameRawR
  ) where

import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Classes.Entity
import Arkham.Difficulty
import Arkham.Game
import Arkham.Id
import Arkham.Investigator
import Arkham.Message
import Conduit
import Control.Lens ( view )
import Control.Monad.Random ( mkStdGen )
import Control.Monad.Random.Class ( getRandom )
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable ( for )
import Database.Esqueleto.Experimental hiding ( update )
import Entity.Arkham.Player
import Import hiding ( delete, on, (==.) )
import Json
import Network.WebSockets ( ConnectionException )
import Safe ( fromJustNote )
import UnliftIO.Exception hiding ( Handler )
import Yesod.WebSockets

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream gameId = catchingConnectionException $ do
  writeChannel <- lift $ getChannel gameId
  gameChannelClients <- appGameChannelClients <$> getYesod
  atomicModifyIORef' gameChannelClients
    $ \channelClients -> (Map.insertWith (+) gameId 1 channelClients, ())
  bracket (atomically $ dupTChan writeChannel) closeConnection
    $ \readChannel -> race_
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
    when (clientCount == 0)
      $ atomicModifyIORef' gameChannelsRef
      $ \gameChannels' -> (Map.delete gameId gameChannels', ())

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ tshow (e :: ConnectionException)

data GetGameJson = GetGameJson
  { investigatorId :: Maybe InvestigatorId
  , multiplayerMode :: MultiplayerVariant
  , game :: PublicGame ArkhamGameId
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

getApiV1ArkhamGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameR gameId = do
  webSockets (gameStream gameId)
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  ArkhamPlayer {..} <- runDB $ entityVal <$> getBy404
    (UniquePlayer userId gameId)
  let
    Game {..} = arkhamGameCurrentData ge
    investigatorId = case arkhamGameMultiplayerVariant ge of
      Solo -> coerce gameActiveInvestigatorId
      WithFriends -> coerce arkhamPlayerInvestigatorId
  pure $ GetGameJson
    (Just investigatorId)
    (arkhamGameMultiplayerVariant ge)
    (PublicGame
      gameId
      (arkhamGameName ge)
      (arkhamGameLog ge)
      (arkhamGameCurrentData ge)
    )

getApiV1ArkhamGameSpectateR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameSpectateR gameId = do
  webSockets $ gameStream gameId
  ge <- runDB $ get404 gameId
  let
    Game {..} = arkhamGameCurrentData ge
    investigatorId = coerce gameActiveInvestigatorId
  pure $ GetGameJson
    (Just investigatorId)
    (arkhamGameMultiplayerVariant ge)
    (PublicGame
      gameId
      (arkhamGameName ge)
      (arkhamGameLog ge)
      (arkhamGameCurrentData ge)
    )

getApiV1ArkhamGamesR :: Handler [PublicGame ArkhamGameId]
getApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  games <- runDB $ select $ do
    (players :& games) <-
      from
      $ table @ArkhamPlayer
      `InnerJoin` table @ArkhamGame
      `on` (\(players :& games) ->
             players ^. ArkhamPlayerArkhamGameId ==. games ^. persistIdField
           )
    where_ (players ^. ArkhamPlayerUserId ==. val userId)
    orderBy [desc $ games ^. ArkhamGameUpdatedAt]
    pure games
  pure $ map toPublicGame games

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
  deriving anyclass FromJSON

getApiV1ArkhamGameExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameExportR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  players <- runDB $ select $ do
    players <- from $ table @ArkhamPlayer
    where_ (players ^. ArkhamPlayerArkhamGameId ==. val gameId)
    pure players
  pure $ ArkhamExport
    { aeCampaignPlayers = map (arkhamPlayerInvestigatorId . entityVal) players
    , aeCampaignData = arkhamGameToExportData ge
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
        gameId <- insert $ ArkhamGame
          agedName
          agedCurrentData
          agedChoices
          agedLog
          Solo
          now
          now
        traverse_ (insert_ . ArkhamPlayer userId gameId) investigatorIds
        pure gameId
      pure $ toPublicGame $ Entity
        key
        (ArkhamGame agedName agedCurrentData agedChoices agedLog Solo now now)

postApiV1ArkhamGamesR :: Handler (PublicGame ArkhamGameId)
postApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  CreateGamePost {..} <- requireCheckJsonBody
  investigators <- for (catMaybes deckIds) $ \deckId -> do
    deck <- runDB $ get404 deckId
    when (arkhamDeckUserId deck /= userId) notFound
    (iid, decklist) <- liftIO $ loadDecklist deck
    pure (lookupInvestigator iid, decklist)
  let
    investigatorId =
      coerce . toId . fst . fromJustNote "must have one investigator" $ headMay
        investigators
  newGameSeed <- liftIO getRandom
  genRef <- newIORef (mkStdGen newGameSeed)
  now <- liftIO getCurrentTime
  case campaignId of
    Just cid -> do
      (queueRef, game) <- liftIO
        $ newCampaign cid newGameSeed playerCount investigators difficulty
      gameRef <- newIORef game
      runGameApp
        (GameApp gameRef queueRef genRef $ pure . const ())
        (runMessages Nothing)
      ge <- readIORef gameRef
      updatedQueue <- readIORef queueRef
      key <- runDB $ do
        gameId <- insert $ ArkhamGame
          campaignName
          ge
          [Choice mempty updatedQueue]
          []
          multiplayerVariant
          now
          now
        insert_ $ ArkhamPlayer userId gameId investigatorId
        pure gameId
      pure $ toPublicGame $ Entity
        key
        (ArkhamGame
          campaignName
          ge
          [Choice mempty updatedQueue]
          []
          multiplayerVariant
          now
          now
        )
    Nothing -> case scenarioId of
      Just sid -> do
        (queueRef, game) <- liftIO
          $ newScenario sid newGameSeed playerCount investigators difficulty
        gameRef <- newIORef game
        runGameApp
          (GameApp gameRef queueRef genRef $ pure . const ())
          (runMessages Nothing)
        ge <- readIORef gameRef
        let
          diffDown = diff ge game
        updatedQueue <- readIORef queueRef
        key <- runDB $ do
          gameId <- insert $ ArkhamGame
            campaignName
            ge
            [Choice diffDown updatedQueue]
            []
            multiplayerVariant
            now
            now
          insert_ $ ArkhamPlayer userId gameId investigatorId
          pure gameId
        pure $ toPublicGame $ Entity
          key
          (ArkhamGame
            campaignName
            ge
            [Choice diffDown updatedQueue]
            []
            multiplayerVariant
            now
            now
          )
      Nothing -> error "missing either campaign id or scenario id"

data Answer
  = Answer QuestionResponse
  | PaymentAmountsAnswer PaymentAmountsResponse
  | AmountsAnswer AmountsResponse
  deriving stock Generic
  deriving anyclass FromJSON

data QuestionResponse = QuestionResponse
  { qrChoice :: Int
  , qrInvestigatorId :: Maybe InvestigatorId
  }
  deriving stock Generic

newtype PaymentAmountsResponse = PaymentAmountsResponse
  { parAmounts :: HashMap InvestigatorId Int }
  deriving stock Generic

newtype AmountsResponse = AmountsResponse
  { arAmounts :: HashMap Text Int }
  deriving stock Generic

instance FromJSON QuestionResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "qr"

instance FromJSON PaymentAmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "par"

instance FromJSON AmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ar"

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [ x | (i, x) <- zip [0 ..] xs, i /= n ])

putApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  Entity pid arkhamPlayer@ArkhamPlayer {..} <- runDB
    $ getBy404 (UniquePlayer userId gameId)
  let
    gameJson@Game {..} = arkhamGameCurrentData
    investigatorId = fromMaybe
      (coerce arkhamPlayerInvestigatorId)
      (answerInvestigator response)
    messages = handleAnswer gameJson investigatorId response

  let currentQueue = maybe [] choiceMessages $ headMay arkhamGameChoices

  gameRef <- newIORef gameJson
  queueRef <- newIORef (messages <> currentQueue)
  logRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId
  runGameApp
    (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel))
    (runMessages Nothing)
  ge <- readIORef gameRef
  let
    diffDown = diff ge arkhamGameCurrentData

  updatedQueue <- readIORef queueRef
  updatedLog <- (arkhamGameLog <>) <$> readIORef logRef
  now <- liftIO getCurrentTime
  void $ runDB $ do
    replace gameId $ ArkhamGame
      arkhamGameName
      ge
      (Choice diffDown updatedQueue : arkhamGameChoices)
      updatedLog
      arkhamGameMultiplayerVariant
      arkhamGameCreatedAt
      now
    case arkhamGameMultiplayerVariant of
      Solo -> replace pid $ arkhamPlayer
        { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
        }
      WithFriends -> pure ()

  atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate $ PublicGame gameId arkhamGameName updatedLog ge)

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

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
  let
    gameJson@Game {..} = arkhamGameCurrentData
    message = gameMessage response
  let currentQueue = maybe [] choiceMessages $ headMay arkhamGameChoices
  gameRef <- newIORef gameJson
  queueRef <- newIORef (message : currentQueue)
  logRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId
  runGameApp
    (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel))
    (runMessages Nothing)
  ge <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  let
    diffDown = diff ge arkhamGameCurrentData
  updatedLog <- (arkhamGameLog <>) <$> readIORef logRef
  atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate $ PublicGame gameId arkhamGameName updatedLog ge)
  now <- liftIO getCurrentTime
  void $ runDB
    (replace
      gameId
      (ArkhamGame
        arkhamGameName
        ge
        (Choice diffDown updatedQueue : arkhamGameChoices)
        updatedLog
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now
      )
    )

deleteApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
deleteApiV1ArkhamGameR gameId = void $ runDB $ do
  delete $ do
    players <- from $ table @ArkhamPlayer
    where_ $ players ^. ArkhamPlayerArkhamGameId ==. val gameId
  delete $ do
    games <- from $ table @ArkhamGame
    where_ $ games ^. persistIdField ==. val gameId

answerInvestigator :: Answer -> Maybe InvestigatorId
answerInvestigator = \case
  Answer response -> qrInvestigatorId response
  AmountsAnswer _ -> Nothing
  PaymentAmountsAnswer _ -> Nothing

handleAnswer :: Game -> InvestigatorId -> Answer -> [Message]
handleAnswer Game {..} investigatorId = \case
  AmountsAnswer response -> case HashMap.lookup investigatorId gameQuestion of
    Just (ChooseAmounts _ _ _ target) ->
      [ ResolveAmounts
          investigatorId
          (HashMap.toList $ arAmounts response)
          target
      ]
    Just (QuestionLabel _ (ChooseAmounts _ _ _ target)) ->
      [ ResolveAmounts
          investigatorId
          (HashMap.toList $ arAmounts response)
          target
      ]
    _ -> error "Wrong question type"
  PaymentAmountsAnswer response ->
    case HashMap.lookup investigatorId gameQuestion of
      Just (ChoosePaymentAmounts _ _ info) ->
        let
          costMap = HashMap.fromList
            $ map (\(PaymentAmountChoice iid _ _ cost) -> (iid, cost)) info
        in
          concatMap
              (\(iid, n) ->
                replicate n (HashMap.findWithDefault Noop iid costMap)
              )
            $ HashMap.toList (parAmounts response)
      _ -> error "Wrong question type"
  Answer response ->
    let
      q = fromJustNote
        "Invalid question type"
        (HashMap.lookup investigatorId gameQuestion)
    in go id q response
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
        (Just m', msgs'') -> if n - 1 == 0
          then [uiToRun m']
          else [uiToRun m', Ask investigatorId $ f $ ChooseN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseN n msgs'']
    ChooseUpToN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m'@(Done _), _) -> [uiToRun m']
        (Just m', msgs'') -> if n - 1 == 0
          then [uiToRun m']
          else
            [uiToRun m', Ask investigatorId $ f $ ChooseUpToN (n - 1) msgs'']
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
    _ -> error "Wrong question type"

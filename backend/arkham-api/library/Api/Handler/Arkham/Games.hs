{-# LANGUAGE TemplateHaskell #-}
module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , getApiV1ArkhamGamesR
  , postApiV1ArkhamGamesR
  , putApiV1ArkhamGameR
  , putApiV1ArkhamGameRawR
  )
where

import Api.Arkham.Helpers
import Arkham.Types.CampaignId
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.UUID
import Database.Esqueleto
import Entity.Arkham.Player
import Import hiding (on, (==.))
import Json
import Network.WebSockets (ConnectionException)
import Safe (fromJustNote)
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
    when (clientCount == 0) $ do
      atomicModifyIORef' gameChannelsRef
        $ \gameChannels' -> (Map.delete gameId gameChannels', ())

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ pack $ show (e :: ConnectionException)

data GetGameJson = GetGameJson { investigatorId :: InvestigatorId, game :: Entity ArkhamGame }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

getApiV1ArkhamGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  webSockets (gameStream gameId)
  let
    Game {..} = arkhamGameCurrentData ge
    investigatorId = fromJustNote "not in game"
      $ HashMap.lookup (fromIntegral $ fromSqlKey userId) _gamePlayers
  pure $ GetGameJson investigatorId (Entity gameId ge)

getApiV1ArkhamGamesR :: Handler [Entity ArkhamGame]
getApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ select $ from $ \(players `InnerJoin` games) -> do
    on (players ^. ArkhamPlayerArkhamGameId ==. games ^. persistIdField)
    where_ (players ^. ArkhamPlayerUserId ==. val userId)
    pure games

data CreateGamePost = CreateGamePost
  { deckId :: String
  , playerCount :: Int
  , campaignId :: CampaignId
  , difficulty :: Difficulty
  , campaignName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

postApiV1ArkhamGamesR :: Handler (Entity ArkhamGame)
postApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  CreateGamePost {..} <- requireCheckJsonBody
  (iid, deck) <- liftIO $ loadDeck deckId
  let
    hashKey = fromIntegral $ fromSqlKey userId
    investigators = HashMap.singleton hashKey (lookupInvestigator iid, deck)
  ge <-
    liftIO
    $ runMessages
    =<< newCampaign campaignId playerCount investigators difficulty
  key <- runDB $ do
    gameId <- insert $ ArkhamGame campaignName ge
    insert_ $ ArkhamPlayer userId gameId
    pure gameId
  pure $ Entity key (ArkhamGame campaignName ge)

data QuestionReponse = QuestionResponse { qrChoice :: Int, qrGameHash :: UUID }
  deriving stock (Generic)

instance FromJSON QuestionReponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "qr"

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [ x | (i, x) <- zip [0 ..] xs, i /= n ])

putApiV1ArkhamGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
putApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  let
    gameJson@Game {..} = arkhamGameCurrentData
    investigatorId = fromJustNote "not in game"
      $ HashMap.lookup (fromIntegral $ fromSqlKey userId) _gamePlayers
    messages = case HashMap.lookup investigatorId _gameQuestion of
      Just (ChooseOne qs) -> case qs !!? qrChoice response of
        Nothing -> [Ask investigatorId $ ChooseOne qs]
        Just msg -> [msg]
      Just (ChooseOneAtATime msgs) -> do
        let (mm, msgs') = extract (qrChoice response) msgs
        case (mm, msgs') of
          (Just m', []) -> [m']
          (Just m', msgs'') ->
            [m', Ask investigatorId $ ChooseOneAtATime msgs'']
          (Nothing, msgs'') -> [Ask investigatorId $ ChooseOneAtATime msgs'']
      _ -> []

  if _gameHash == qrGameHash response
    then do
      ge <- liftIO $ runMessages =<< toInternalGame
        (gameJson { _gameMessages = messages <> _gameMessages })

      writeChannel <- getChannel gameId
      liftIO $ atomically $ writeTChan
        writeChannel
        (encode (Entity gameId (ArkhamGame arkhamGameName ge)))
      Entity gameId (ArkhamGame arkhamGameName ge)
        <$ runDB (replace gameId (ArkhamGame arkhamGameName ge))
    else invalidArgs ["Hash mismatch"]


newtype PutRawGameJson = PutRawGameJson { gameJson :: GameExternal }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  void $ fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  ge <- liftIO $ runMessages =<< toInternalGame
    ((gameJson response)
      { _gameMessages = Continue "edited" : _gameMessages (gameJson response)
      }
    )
  writeChannel <- getChannel gameId
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode (Entity gameId (ArkhamGame arkhamGameName ge)))
  runDB (replace gameId (ArkhamGame arkhamGameName ge))

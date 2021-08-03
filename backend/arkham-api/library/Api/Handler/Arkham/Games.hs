{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , getApiV1ArkhamGamesR
  , postApiV1ArkhamGamesR
  , putApiV1ArkhamGameR
  , deleteApiV1ArkhamGameR
  , putApiV1ArkhamGameRawR
  ) where

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Arkham.Prelude ((!!?))
import Arkham.Types.CampaignId
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes.Entity
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.ScenarioId
import Control.Lens (view, (%~), (&))
import Control.Monad.Random (mkStdGen)
import Control.Monad.Random.Class (getRandom)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Database.Esqueleto.Experimental hiding (update)
import Entity.Arkham.Player
import Import hiding (delete, on, (==.))
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
    when (clientCount == 0)
      $ atomicModifyIORef' gameChannelsRef
      $ \gameChannels' -> (Map.delete gameId gameChannels', ())

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ pack $ show (e :: ConnectionException)

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
    investigatorId = if arkhamGameMultiplayerVariant ge == Solo
      then coerce gameActiveInvestigatorId
      else coerce arkhamPlayerInvestigatorId
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
  games <- runDB
    (select $ do
      (players :& games) <-
        from
        $ table @ArkhamPlayer
        `InnerJoin` table @ArkhamGame
        `on` (\(players :& games) ->
               players ^. ArkhamPlayerArkhamGameId ==. games ^. persistIdField
             )
      where_ (players ^. ArkhamPlayerUserId ==. val userId)
      pure games
    )
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
  case campaignId of
    Just cid -> do
      (queueRef, game) <- liftIO
        $ newCampaign cid newGameSeed playerCount investigators difficulty
      gameRef <- newIORef game
      runGameApp
        (GameApp gameRef queueRef genRef $ pure . const ())
        (runMessages False)
      ge <- readIORef gameRef
      updatedQueue <- readIORef queueRef
      key <- runDB $ do
        gameId <- insert
          $ ArkhamGame campaignName ge updatedQueue [] multiplayerVariant
        insert_ $ ArkhamPlayer userId gameId investigatorId
        pure gameId
      pure $ toPublicGame $ Entity
        key
        (ArkhamGame campaignName ge updatedQueue [] multiplayerVariant)
    Nothing -> case scenarioId of
      Just sid -> do
        (queueRef, game) <- liftIO
          $ newScenario sid newGameSeed playerCount investigators difficulty
        gameRef <- newIORef game
        runGameApp
          (GameApp gameRef queueRef genRef $ pure . const ())
          (runMessages False)
        ge <- readIORef gameRef
        updatedQueue <- readIORef queueRef
        key <- runDB $ do
          gameId <- insert
            $ ArkhamGame campaignName ge updatedQueue [] multiplayerVariant
          insert_ $ ArkhamPlayer userId gameId investigatorId
          pure gameId
        pure $ toPublicGame $ Entity
          key
          (ArkhamGame campaignName ge updatedQueue [] multiplayerVariant)
      Nothing -> error "missing either campaign id or scenario id"

newtype QuestionReponse = QuestionResponse { qrChoice :: Int }
  deriving stock Generic

instance FromJSON QuestionReponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "qr"

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [ x | (i, x) <- zip [0 ..] xs, i /= n ])

putApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  Entity pid arkhamPlayer@ArkhamPlayer {..} <- runDB
    $ getBy404 (UniquePlayer userId gameId)
  response <- requireCheckJsonBody
  let
    gameJson@Game {..} = arkhamGameCurrentData
    investigatorId = coerce arkhamPlayerInvestigatorId
    messages = case HashMap.lookup investigatorId gameQuestion of
      Just (ChooseOne qs) -> case qs !!? qrChoice response of
        Nothing -> [Ask investigatorId $ ChooseOne qs]
        Just msg -> [msg]
      Just (ChooseN n qs) -> do
        let (mm, msgs') = extract (qrChoice response) qs
        case (mm, msgs') of
          (Just m', []) -> [m']
          (Just m', msgs'') -> if n - 1 == 0
            then [m']
            else [m', Ask investigatorId $ ChooseN (n - 1) msgs'']
          (Nothing, msgs'') -> [Ask investigatorId $ ChooseN n msgs'']
      Just (ChooseUpToN n qs) -> do
        let (mm, msgs') = extract (qrChoice response) qs
        case (mm, msgs') of
          (Just m', []) -> [m']
          (Just m'@(Done _), _) -> [m']
          (Just m', msgs'') -> if n - 1 == 0
            then [m']
            else [m', Ask investigatorId $ ChooseUpToN (n - 1) msgs'']
          (Nothing, msgs'') -> [Ask investigatorId $ ChooseUpToN n msgs'']
      Just (ChooseOneAtATime msgs) -> do
        let (mm, msgs') = extract (qrChoice response) msgs
        case (mm, msgs') of
          (Just m', []) -> [m']
          (Just m', msgs'') ->
            [m', Ask investigatorId $ ChooseOneAtATime msgs'']
          (Nothing, msgs'') -> [Ask investigatorId $ ChooseOneAtATime msgs'']
      Just (ChooseSome msgs) -> do
        let (mm, msgs') = extract (qrChoice response) msgs
        case (mm, msgs') of
          (Just (Done _), _) -> []
          (Just m', msgs'') -> case msgs'' of
            [] -> [m']
            [Done _] -> [m']
            rest -> [m', Ask investigatorId $ ChooseSome rest]
          (Nothing, msgs'') -> [Ask investigatorId $ ChooseSome msgs'']
      _ -> []

  gameRef <- newIORef
    (gameJson & choicesL %~ (AskChoice investigatorId (qrChoice response) :))
  queueRef <- newIORef (messages <> arkhamGameQueue)
  logRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId
  runGameApp
    (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel))
    (runMessages False)
  ge <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  updatedLog <- (arkhamGameLog <>) <$> readIORef logRef
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate $ PublicGame gameId arkhamGameName updatedLog ge)
  void $ runDB $ do
    replace gameId $ ArkhamGame
      arkhamGameName
      ge
      updatedQueue
      updatedLog
      arkhamGameMultiplayerVariant
    replace pid $ arkhamPlayer
      { arkhamPlayerInvestigatorId = coerce (view activeInvestigatorIdL ge)
      }

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

handleMessageLog
  :: MonadIO m => IORef [Text] -> TChan BSL.ByteString -> Message -> m ()
handleMessageLog logRef writeChannel msg = case toHumanReadable msg of
  Nothing -> pure ()
  Just readableMsg -> liftIO $ do
    atomicModifyIORef' logRef (\logs -> (logs <> [readableMsg], ()))
    atomically $ writeTChan writeChannel (encode $ GameMessage readableMsg)

putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  void $ fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  let
    gameJson@Game {..} = arkhamGameCurrentData
    message = gameMessage response
  gameRef <- newIORef (gameJson & choicesL %~ (DebugMessage message :))
  queueRef <- newIORef (message : arkhamGameQueue)
  logRef <- newIORef []
  genRef <- newIORef (mkStdGen gameSeed)
  writeChannel <- getChannel gameId
  runGameApp
    (GameApp gameRef queueRef genRef (handleMessageLog logRef writeChannel))
    (runMessages False)
  ge <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  updatedLog <- (arkhamGameLog <>) <$> readIORef logRef
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate $ PublicGame gameId arkhamGameName updatedLog ge)
  void $ runDB
    (replace
      gameId
      (ArkhamGame
        arkhamGameName
        ge
        updatedQueue
        updatedLog
        arkhamGameMultiplayerVariant
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

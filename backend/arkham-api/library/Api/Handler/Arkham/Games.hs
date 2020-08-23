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
import Arkham.Types.GameJson
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import qualified Data.HashMap.Strict as HashMap
import Data.UUID
import Data.UUID.V4
import Database.Esqueleto
import Entity.Arkham.GameSetupState
import Entity.Arkham.PendingGame
import Entity.Arkham.Player
import Import hiding (on, (==.))
import Json
import Safe (fromJustNote)
import Yesod.WebSockets

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream _ = do
  App { appBroadcastChannel = writeChannel } <- getYesod
  readChannel <- atomically $ dupTChan writeChannel
  race_
    (forever $ atomically (readTChan readChannel) >>= sendTextData)
    (runConduit $ sourceWS .| mapM_C (atomically . writeTChan writeChannel))

data GetGameJson = GetGameJson { investigatorId :: InvestigatorId, game :: Entity ArkhamGame }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

getApiV1ArkhamGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ge <- runDB $ get404 gameId
  webSockets (gameStream gameId)
  let
    GameJson {..} = arkhamGameCurrentData ge
    investigatorId = fromJustNote "not in game"
      $ HashMap.lookup (fromIntegral $ fromSqlKey userId) gPlayers
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

postApiV1ArkhamGamesR
  :: Handler (Either (Entity ArkhamPendingGame) (Entity ArkhamGame))
postApiV1ArkhamGamesR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  CreateGamePost {..} <- requireCheckJsonBody
  (iid, deck) <- liftIO $ loadDeck deckId
  let
    hashKey = fromIntegral $ fromSqlKey userId
    investigators = HashMap.singleton hashKey (lookupInvestigator iid, deck)
  if playerCount == 1
    then do
      ge <-
        liftIO $ runMessages =<< newCampaign campaignId investigators difficulty
      key <- runDB $ do
        gameId <- insert $ ArkhamGame ge
        insert_ $ ArkhamPlayer userId gameId
        pure gameId
      pure $ Right (Entity key (ArkhamGame ge))
    else do
      let
        apg = GameSetupState
          playerCount
          (HashMap.singleton hashKey deckId)
          campaignId
          difficulty
      token <- liftIO nextRandom
      key <- runDB $ insert $ ArkhamPendingGame token apg
      pure $ Left (Entity key (ArkhamPendingGame token apg))

data QuestionReponse = QuestionResponse { choice :: Int, gameHash :: UUID }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [ x | (i, x) <- zip [0 ..] xs, i /= n ])

putApiV1ArkhamGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
putApiV1ArkhamGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  game <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  let
    gameJson@GameJson {..} = arkhamGameCurrentData game
    investigatorId = fromJustNote "not in game"
      $ HashMap.lookup (fromIntegral $ fromSqlKey userId) gPlayers
    messages = case HashMap.lookup investigatorId gQuestion of
      Just (ChooseOne qs) -> case qs !!? choice response of
        Nothing -> [Ask investigatorId $ ChooseOne qs]
        Just msg -> [msg]
      Just (ChooseOneAtATime msgs) -> do
        let (mm, msgs') = extract (choice response) msgs
        case (mm, msgs') of
          (Just m', []) -> [m']
          (Just m', msgs'') ->
            [m', Ask investigatorId $ ChooseOneAtATime msgs'']
          (Nothing, msgs'') -> [Ask investigatorId $ ChooseOneAtATime msgs'']
      _ -> []

  if gHash == gameHash response
    then do
      ge <- liftIO $ runMessages =<< toInternalGame
        (gameJson { gMessages = messages <> gMessages })

      App { appBroadcastChannel = writeChannel } <- getYesod
      liftIO $ atomically $ writeTChan
        writeChannel
        (encode (Entity gameId (ArkhamGame ge)))
      Entity gameId (ArkhamGame ge) <$ runDB (replace gameId (ArkhamGame ge))
    else invalidArgs ["Hash mismatch"]


newtype PutRawGameJson = PutRawGameJson { gameJson :: GameJson }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  void $ fromJustNote "Not authenticated" <$> getRequestUserId
  void $ runDB $ get404 gameId
  response <- requireCheckJsonBody
  ge <- liftIO $ runMessages =<< toInternalGame
    ((gameJson response)
      { gMessages = Continue "edited" : gMessages (gameJson response)
      }
    )
  App { appBroadcastChannel = writeChannel } <- getYesod
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode (Entity gameId (ArkhamGame ge)))
  runDB (replace gameId (ArkhamGame ge))

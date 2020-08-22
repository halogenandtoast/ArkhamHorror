module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , getApiV1ArkhamGamesR
  , postApiV1ArkhamGamesR
  , putApiV1ArkhamGameR
  , putApiV1ArkhamGameRawR
  )
where

import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.GameJson
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4
import Database.Esqueleto
import Entity.Arkham.Player
import GHC.Stack
import Import hiding (on, (==.))
import Network.HTTP.Conduit (simpleHttp)
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
  { deckIds :: [String]
  , campaignId :: CampaignId
  , difficulty :: Difficulty
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

postApiV1ArkhamGamesR :: Handler (Entity ArkhamGame)
postApiV1ArkhamGamesR = do
  CreateGamePost {..} <- requireCheckJsonBody
  investigators <-
    (HashMap.fromList <$>) $ for (zip [1 ..] deckIds) $ \(userId, deckId) -> do
      (iid, deck) <- liftIO $ loadDeck deckId
      pure (userId, (lookupInvestigator iid, deck))
  ge <- liftIO $ runMessages =<< newCampaign campaignId investigators difficulty
  key <- runDB $ do
    gameId <- insert $ ArkhamGame ge
    for_ (HashMap.keys investigators) $ \userId ->
      insert $ ArkhamPlayer (toSqlKey $ fromIntegral userId) gameId
    pure gameId
  pure (Entity key (ArkhamGame ge))

newtype QuestionReponse = QuestionResponse { choice :: Int }
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
  ge <- liftIO $ runMessages =<< toInternalGame
    (gameJson { gMessages = messages <> gMessages })

  App { appBroadcastChannel = writeChannel } <- getYesod
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode (Entity gameId (ArkhamGame ge)))
  Entity gameId (ArkhamGame ge) <$ runDB (replace gameId (ArkhamGame ge))


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

data ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: HashMap CardCode Int, investigator_code :: InvestigatorId }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadDeck :: HasCallStack => String -> IO (InvestigatorId, [PlayerCard])
loadDeck deckId = do
  edecklist <- eitherDecode @ArkhamDBDecklist
    <$> simpleHttp ("https://arkhamdb.com/api/public/decklist/" <> deckId)
  case edecklist of
    Left err -> throwString $ "Parsing failed with: " <> err
    Right decklist -> do
      cards <-
        flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
          if cardCode /= "01000"
            then replicateM
              count'
              ((<$> (CardId <$> nextRandom)) (lookupPlayerCard cardCode))
            else pure []
      pure (investigator_code decklist, cards)

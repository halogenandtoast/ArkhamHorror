module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , postApiV1ArkhamCreateGameR
  , putApiV1ArkhamGameR
  , putApiV1ArkhamGameRawR
  )
where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Game
import Arkham.Types.GameJson
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4
import Database.Persist.Sql
import GHC.Stack
import Import
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

postApiV1ArkhamCreateGameR :: Handler (Entity ArkhamGame)
postApiV1ArkhamCreateGameR = do
  (iid1, deck1) <- liftIO $ loadDeck "20344"
  -- (iid2, deck2) <- liftIO $ loadDeck "101"
  ge <- liftIO $ runMessages =<< newGame
    "01104"
    (HashMap.fromList [(1, (lookupInvestigator iid1, deck1))
      -- , (2, (lookupInvestigator iid2, deck2))
                                                            ])
  key <- runDB $ insert $ ArkhamGame ge
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
  App { appBroadcastChannel = writeChannel } <- getYesod
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode (Entity gameId (ArkhamGame $ gameJson response)))
  runDB (replace gameId (ArkhamGame $ gameJson response))

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

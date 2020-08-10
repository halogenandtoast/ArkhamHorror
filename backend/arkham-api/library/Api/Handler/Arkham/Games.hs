module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , postApiV1ArkhamCreateGameR
  , putApiV1ArkhamGameR
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
import GHC.Stack
import Import
import Network.HTTP.Conduit (simpleHttp)
import Yesod.WebSockets

gameStream :: ArkhamGameId -> WebSocketsT Handler ()
gameStream _ = do
  App { appBroadcastChannel = writeChannel } <- getYesod
  readChannel <- atomically $ dupTChan writeChannel
  race_
    (forever $ atomically (readTChan readChannel) >>= sendTextData)
    (sourceWS $$ mapM_C (atomically . writeTChan writeChannel))

getApiV1ArkhamGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
getApiV1ArkhamGameR gameId = do
  ge <- runDB $ get404 gameId
  webSockets (gameStream gameId)
  pure (Entity gameId ge)

postApiV1ArkhamCreateGameR :: Handler (Entity ArkhamGame)
postApiV1ArkhamCreateGameR = do
  (iid1, deck1) <- liftIO $ loadDeck "20344"
  (iid2, deck2) <- liftIO $ loadDeck "101"
  (_, ge) <- liftIO $ runMessages =<< newGame
    "01104"
    (HashMap.fromList
      [ (1, (lookupInvestigator iid1, deck1))
      , (2, (lookupInvestigator iid2, deck2))
      ]
    )
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
  game <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  let
    gameJson@GameJson {..} = arkhamGameCurrentData game
    messages = case gQuestion of
      Just (ChooseOne qs) -> case qs !!? choice response of
        Nothing -> [Ask $ ChooseOne qs]
        Just msg -> [msg]
      Just (ChooseOneAtATime msgs) -> do
        let (mm, msgs') = extract (choice response) msgs
        case (mm, msgs') of
          (Just m', []) -> [m']
          (Just m', msgs'') -> [m', Ask $ ChooseOneAtATime msgs'']
          (Nothing, msgs'') -> [Ask $ ChooseOneAtATime msgs'']
      Just (ChooseOneFromSource MkChooseOneFromSource {..}) ->
        maybeToList (map unlabel chooseOneChoices !!? choice response)
      _ -> []
  (_, ge) <- liftIO $ runMessages =<< toInternalGame
    (gameJson { gMessages = messages <> gMessages })

  App { appBroadcastChannel = writeChannel } <- getYesod
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode (Entity gameId (ArkhamGame ge)))
  Entity gameId (ArkhamGame ge) <$ runDB (replace gameId (ArkhamGame ge))

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

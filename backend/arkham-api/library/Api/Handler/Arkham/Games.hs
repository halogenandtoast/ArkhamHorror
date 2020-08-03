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
import Arkham.Types.Message
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4
import GHC.Stack
import Import
import Network.HTTP.Conduit (simpleHttp)

getApiV1ArkhamGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
getApiV1ArkhamGameR gameId = do
  ge <- runDB $ get404 gameId
  pure (Entity gameId ge)

postApiV1ArkhamCreateGameR :: Handler (Entity ArkhamGame)
postApiV1ArkhamCreateGameR = do
  deck <- liftIO $ loadDeck "101"
  (_, ge) <- liftIO $ runMessages =<< newGame
    "01104"
    (HashMap.fromList [(1, (lookupInvestigator "01002", deck))])
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
      Just (ChooseOne qs) -> maybeToList (qs !!? choice response)
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
  Entity gameId (ArkhamGame ge) <$ runDB (replace gameId (ArkhamGame ge))

newtype ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: HashMap CardCode Int }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadDeck :: HasCallStack => String -> IO [PlayerCard]
loadDeck deckId = do
  edecklist <- eitherDecode @ArkhamDBDecklist
    <$> simpleHttp ("https://arkhamdb.com/api/public/decklist/" <> deckId)
  case edecklist of
    Left err -> throwString $ "Parsing failed with: " <> err
    Right decklist ->
      flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
        if cardCode /= "01000"
          then replicateM
            count'
            ((<$> (CardId <$> nextRandom)) (lookupPlayerCard cardCode))
          else pure []

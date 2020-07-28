module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , postApiV1ArkhamCreateGameR
  , putApiV1ArkhamGameR
  )
where

import Arkham.Types.Card
import Arkham.Types.Game
import Arkham.Types.Message
import Arkham.Types.Helpers
import Arkham.Types.GameJson
import Arkham.Types.Investigator
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Import
import Network.HTTP.Conduit (simpleHttp)

getApiV1ArkhamGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
getApiV1ArkhamGameR gameId = do
  ge <- runDB $ get404 gameId
  pure (Entity gameId ge)

postApiV1ArkhamCreateGameR :: Handler (Entity ArkhamGame)
postApiV1ArkhamCreateGameR = do
  deck <- liftIO $ loadDeck "20344"
  (_, ge) <- liftIO $ runMessages =<< newGame
    "01104"
    [(lookupInvestigator "01001", deck)]
  key <- runDB $ insert $ ArkhamGame ge
  pure (Entity key (ArkhamGame ge))

newtype QuestionReponse = QuestionResponse { choice :: Int }
  deriving stock (Generic)
  deriving anyclass (FromJSON)


putApiV1ArkhamGameR :: ArkhamGameId -> Handler GameJson
putApiV1ArkhamGameR gameId = do
  game <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  let gameJson@GameJson{..} = arkhamGameCurrentData game
      messages =
        case gQuestion of
          Just (ChooseOne qs) -> do
            maybeToList $ Ask <$> qs !!? (choice response)
          _ -> []
  (_, ge) <- liftIO $ runMessages =<< toInternalGame (gameJson { gMessages = messages <> gMessages })
  ge <$ runDB (replace gameId (ArkhamGame ge))

  

  

newtype ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: HashMap CardCode Int }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadDeck :: String -> IO [PlayerCard]
loadDeck deckId = do
  edecklist <- eitherDecode @ArkhamDBDecklist
    <$> simpleHttp ("https://arkhamdb.com/api/public/decklist/" <> deckId)
  case edecklist of
    Left err -> throwString $ "Parsing failed with: " <> err
    Right decklist ->
      pure $ flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
        do
          guard $ cardCode /= "01000"
          replicate count' (lookupPlayerCard cardCode)

module Api.Handler.Arkham.Games
  ( getApiV1ArkhamGameR
  , postApiV1ArkhamCreateGameR
  , putApiV1ArkhamGameR
  )
where

import Arkham.Types.Card
import Arkham.Types.Game
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

putApiV1ArkhamGameR :: ArkhamGameId -> Handler GameJson
putApiV1ArkhamGameR = runDB . (arkhamGameCurrentData <$>) . get404

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

module Api.Arkham.Helpers where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.InvestigatorId
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4
import GHC.Stack
import Json
import Network.HTTP.Conduit (simpleHttp)

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

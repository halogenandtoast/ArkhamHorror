module Main where

import Arkham.Types.Card
import Arkham.Types.Game
import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Conduit (simpleHttp)
import Text.Pretty.Simple

loadDeck :: String -> IO [PlayerCard]
loadDeck deckId = do
  edecklist <- eitherDecode @ArkhamDBDecklist
    <$> simpleHttp ("https://arkhamdb.com/api/public/decklist/" <> deckId)
  case edecklist of
    Left err -> throwString $ "Parsing failed with: " <> err
    Right decklist ->
      pure $ flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count ->
        do
          guard $ cardCode /= "01000"
          replicate count (lookupPlayerCard cardCode)

newtype ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: HashMap CardCode Int }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

main :: IO ()
main = do
  deck <- loadDeck "20344"
  ge <- runGame =<< newGame "01104" [(lookupInvestigator "01001", deck)]
  pPrint ge

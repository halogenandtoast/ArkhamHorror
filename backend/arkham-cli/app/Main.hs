{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Game
import Arkham.Types.GameJson
import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Network.HTTP.Conduit (simpleHttp)
import System.Environment
import Text.Pretty.Simple
import Text.Read (readMaybe)

instance FromField GameJson where
  fromField = fromJSONField

loadFromDB :: Int -> IO GameJson
loadFromDB gid = do
  conn <- connectPostgreSQL "dbname=arkham-horror-backend"
  [Only i] <- query
    conn
    "select current_data from arkham_games where id = ?"
    [gid]
  pure i

loadDeck :: String -> IO [PlayerCard]
loadDeck deckId = do
  edecklist <- eitherDecode @ArkhamDBDecklist
    <$> simpleHttp ("https://arkhamdb.com/api/public/decklist/" <> deckId)
  pPrint edecklist
  case edecklist of
    Left err -> throwString $ "Parsing failed with: " <> err
    Right decklist ->
      flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
        if cardCode /= "01000"
          then replicateM
            count'
            ((<$> (CardId <$> nextRandom)) (lookupPlayerCard cardCode))
          else pure []

newtype ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: HashMap CardCode Int }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

main :: IO ()
main = do
  mdbid <- lookupEnv "GAME_ID"
  deckId <- fromMaybe "20344" <$> lookupEnv "DECK_ID"
  case readMaybe @Int =<< mdbid of
    Nothing -> do
      deck <- loadDeck deckId
      pPrint deck
      ge <- runGame =<< newGame
        "01104"
        (HashMap.fromList [(1, (lookupInvestigator "01001", deck))])
      pPrint ge
    Just gid -> do
      gj <- loadFromDB gid
      ge <- runGame =<< toInternalGame gj
      pPrint ge

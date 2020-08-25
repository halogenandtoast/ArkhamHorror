{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.GameJson
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
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

keepAsking :: forall a m . (Show a, Read a, MonadIO m) => String -> m a
keepAsking s = do
  putStr $ pack s
  liftIO $ hFlush stdout
  mresult <- readMaybe @a . unpack <$> getLine
  case mresult of
    Nothing -> keepAsking s
    Just a -> pure a

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? (n - 1) in (a, [ x | (i, x) <- zip [1 ..] xs, i /= n ])

handleQuestion :: MonadIO m => InvestigatorId -> Question -> m [Message]
handleQuestion iid = \case
  ChooseOne [] -> pure []
  ChooseOne msgs -> do
    i <- keepAsking @Int
      ("Choose one:\n\n" <> unlines (map show $ zip @_ @Int [1 ..] msgs))
    pure . maybeToList $ msgs !!? (i - 1)
  ChooseOneAtATime [] -> pure []
  ChooseOneAtATime msgs -> do
    i <- keepAsking @Int
      ("Choose one at a time:\n\n"
      <> unlines (map show $ zip @_ @Int [1 ..] msgs)
      )
    let (mm, msgs') = extract i msgs
    case mm of
      Just m' -> pure [m', Ask iid $ ChooseOneAtATime msgs']
      Nothing -> pure []

runGame :: MonadIO m => Game -> m GameJson
runGame g = do
  let ref = giMessages g
  gameJson <- runMessages g
  pPrint gameJson
  case HashMap.toList (gQuestion gameJson) of
    [(investigatorId, question)] -> do
      messages <- handleQuestion investigatorId question
      modifyIORef' ref (messages <>)
      messages' <- readIORef ref
      if null messages'
        then pure gameJson
        else runGame $ toInternalGame' ref gameJson
    _ -> pure gameJson

main :: IO ()
main = do
  mdbid <- lookupEnv "GAME_ID"
  deckId <- fromMaybe "20344" <$> lookupEnv "DECK_ID"
  investigatorId <- maybe "01001" (InvestigatorId . CardCode . pack)
    <$> lookupEnv "INVESTIGATOR_ID"
  case readMaybe @Int =<< mdbid of
    Nothing -> do
      deck <- loadDeck deckId
      pPrint deck
      ge <-
        runGame
          =<< newCampaign
                "01"
                1
                (HashMap.fromList
                  [(1, (lookupInvestigator investigatorId, deck))]
                )
                Easy
      pPrint ge
    Just gid -> do
      gj <- loadFromDB gid
      ge <- runGame =<< toInternalGame gj
      pPrint ge

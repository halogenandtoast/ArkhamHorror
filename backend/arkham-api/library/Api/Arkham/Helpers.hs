{-# LANGUAGE TemplateHaskell #-}
module Api.Arkham.Helpers where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.InvestigatorId
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.UUID.V4
import GHC.Stack
import Import
import Json
import Network.HTTP.Conduit (simpleHttp)

getChannel :: ArkhamGameId -> Handler (TChan BSL.ByteString)
getChannel gameId = do
  gameChannelsRef <- appGameChannels <$> getYesod
  gameChannels <- readIORef gameChannelsRef
  $logInfo (pack $ show $ Map.keys gameChannels)
  case Map.lookup gameId gameChannels of
    Just chan -> pure chan
    Nothing -> do
      chan <- atomically newBroadcastTChan
      atomicModifyIORef' gameChannelsRef
        $ \gameChannels' -> (Map.insert gameId chan gameChannels', ())
      pure chan

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

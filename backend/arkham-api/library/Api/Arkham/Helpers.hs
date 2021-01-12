{-# LANGUAGE TupleSections #-}
module Api.Arkham.Helpers where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.InvestigatorId
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.UUID.V4
import Import
import Json

getChannel :: ArkhamGameId -> Handler (TChan BSL.ByteString)
getChannel gameId = do
  gameChannelsRef <- appGameChannels <$> getYesod
  gameChannels <- readIORef gameChannelsRef
  case Map.lookup gameId gameChannels of
    Just chan -> pure chan
    Nothing -> do
      chan <- atomically newBroadcastTChan
      atomicModifyIORef' gameChannelsRef
        $ \gameChannels' -> (Map.insert gameId chan gameChannels', ())
      pure chan

newtype ArkhamDBDecklistMeta = ArkhamDBDecklistMeta { alternate_front :: InvestigatorId }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadDecklistCards :: ArkhamDBDecklist -> IO [PlayerCard]
loadDecklistCards decklist =
  flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
    if cardCode /= "01000"
      then replicateM
        count'
        ((<$> (CardId <$> nextRandom)) (lookupPlayerCard cardCode))
      else pure []

loadDecklist :: ArkhamDeck -> IO (InvestigatorId, [PlayerCard])
loadDecklist arkhamDeck = (investigatorId, ) <$> loadDecklistCards decklist
 where
  decklist = arkhamDeckList arkhamDeck
  investigatorId = case meta decklist of
    Nothing -> investigator_code decklist
    Just meta' ->
      case decode @ArkhamDBDecklistMeta (encodeUtf8 $ fromStrict meta') of
        Nothing -> investigator_code decklist
        Just ArkhamDBDecklistMeta {..} -> alternate_front

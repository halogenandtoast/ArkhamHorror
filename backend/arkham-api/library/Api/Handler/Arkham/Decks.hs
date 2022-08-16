module Api.Handler.Arkham.Decks
  ( getApiV1ArkhamDecksR
  , postApiV1ArkhamDecksR
  , deleteApiV1ArkhamDeckR
  , putApiV1ArkhamGameDecksR
  , postApiV1ArkhamSyncDeckR
  ) where

import Import hiding ( delete, on, update, (=.), (==.) )

import Api.Arkham.Helpers
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Helpers
import Arkham.Id
import Arkham.Message
import Arkham.PlayerCard
import Control.Monad.Random ( mkStdGen )
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding ( isNothing )
import Json hiding ( Success )
import Network.HTTP.Conduit ( simpleHttp )
import Network.HTTP.Types
import Network.HTTP.Types.Status qualified as Status
import Safe ( fromJustNote )

getApiV1ArkhamDecksR :: Handler [Entity ArkhamDeck]
getApiV1ArkhamDecksR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ select $ do
    decks <- from $ table @ArkhamDeck
    where_ (decks ^. ArkhamDeckUserId ==. val userId)
    pure decks

data CreateDeckPost = CreateDeckPost
  { deckId :: Text
  , deckName :: Text
  , deckUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype UpgradeDeckPost = UpgradeDeckPost
  { udpDeckUrl :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON UpgradeDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"

newtype DeckError = UnimplementedCard CardCode
  deriving stock (Show, Eq, Generic)

instance ToJSON DeckError where
  toJSON = genericToJSON $ defaultOptions { tagSingleConstructors = True }

toDeckErrors :: ArkhamDeck -> [DeckError]
toDeckErrors deck = flip mapMaybe cardCodes $ \cardCode -> maybe
  (Just $ UnimplementedCard cardCode)
  (const Nothing)
  (HashMap.lookup cardCode allPlayerCards)
 where
  decklist = arkhamDeckList deck
  cardCodes = HashMap.keys $ slots decklist

postApiV1ArkhamDecksR :: Handler (Entity ArkhamDeck)
postApiV1ArkhamDecksR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  postData <- requireCheckJsonBody
  edeck <- fromPostData userId postData
  case edeck of
    Left err -> error $ T.pack err
    Right deck -> case toDeckErrors deck of
      [] -> runDB $ insertEntity deck
      err -> sendStatusJSON status400 err

putApiV1ArkhamGameDecksR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameDecksR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  ArkhamPlayer {..} <- runDB $ entityVal <$> getBy404
    (UniquePlayer userId gameId)
  postData <- requireCheckJsonBody
  let
    Game {..} = arkhamGameCurrentData
    investigatorId = coerce arkhamPlayerInvestigatorId
  msg <- case udpDeckUrl postData of
    Nothing -> pure $ chooseOne investigatorId [Label "done" []]
    Just deckUrl -> do
      edecklist <- getDeckList deckUrl
      case edecklist of
        Left err -> error $ show err
        Right decklist -> do
          cards <- liftIO $ loadDecklistCards decklist
          pure $ UpgradeDeck investigatorId (Deck cards)

  let currentQueue :: [Message] = maybe [] choiceMessages $ headMay arkhamGameChoices

  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newIORef (msg : currentQueue)
  genRef <- newIORef (mkStdGen gameSeed)
  runGameApp
    (GameApp gameRef queueRef genRef $ pure . const ())
    (runMessages Nothing)
  ge <- readIORef gameRef

  let
    diffUp = diff arkhamGameCurrentData ge
    diffDown = diff ge arkhamGameCurrentData
  updatedQueue <- readIORef queueRef
  let updatedMessages = []
  writeChannel <- getChannel gameId
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate $ PublicGame gameId arkhamGameName updatedMessages ge)
  now <- liftIO getCurrentTime
  runDB $ replace
    gameId
    (ArkhamGame
      arkhamGameName
      ge
      (Choice diffUp diffDown updatedQueue : arkhamGameChoices)
      updatedMessages
      arkhamGameMultiplayerVariant
      arkhamGameCreatedAt
      now
    )

fromPostData
  :: (MonadIO m) => UserId -> CreateDeckPost -> m (Either String ArkhamDeck)
fromPostData userId CreateDeckPost {..} = do
  edecklist <- getDeckList deckUrl
  pure $ do
    decklist <- edecklist
    pure $ ArkhamDeck
      { arkhamDeckUserId = userId
      , arkhamDeckUrl = deckUrl
      , arkhamDeckInvestigatorName = tshow $ investigator_name decklist
      , arkhamDeckName = deckName
      , arkhamDeckList = decklist
      }

getDeckList :: MonadIO m => Text -> m (Either String ArkhamDBDecklist)
getDeckList url = liftIO $ eitherDecode <$> simpleHttp (T.unpack url)

deleteApiV1ArkhamDeckR :: ArkhamDeckId -> Handler ()
deleteApiV1ArkhamDeckR deckId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ delete $ do
    decks <- from $ table @ArkhamDeck
    where_ $ decks ^. persistIdField ==. val deckId
    where_ $ decks ^. ArkhamDeckUserId ==. val userId

newtype JSONError = JSONError { errorMsg :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


postApiV1ArkhamSyncDeckR :: ArkhamDeckId -> Handler ()
postApiV1ArkhamSyncDeckR deckId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  deck <- runDB $ get404 deckId
  unless (arkhamDeckUserId deck == userId) $ sendStatusJSON
    Status.status400
    (JSONError "Deck does not belong to this user")
  edecklist <- getDeckList (arkhamDeckUrl deck)
  case edecklist of
    Right decklist -> runDB $ update $ \d -> do
      set d [ArkhamDeckList =. val decklist]
      where_ $ d ^. ArkhamDeckId ==. val deckId
    Left _ ->
      sendStatusJSON Status.status400 (JSONError "Could not sync deck")

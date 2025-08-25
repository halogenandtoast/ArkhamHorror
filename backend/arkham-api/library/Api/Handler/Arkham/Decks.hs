module Api.Handler.Arkham.Decks (
  getApiV1ArkhamDecksR,
  getApiV1ArkhamDeckR,
  postApiV1ArkhamDecksR,
  postApiV1ArkhamDecksValidateR,
  deleteApiV1ArkhamDeckR,
  putApiV1ArkhamGameDecksR,
  postApiV1ArkhamSyncDeckR,
) where

import Import hiding (delete, on, update, (=.), (==.))

import Api.Arkham.Helpers
import Arkham.Card.CardCode
import Arkham.Classes.HasQueue
import Arkham.Decklist
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Id
import Arkham.Investigator.Cards (allInvestigatorCards)
import Arkham.Message
import Arkham.PlayerCard
import Arkham.Queue
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (isNothing, (<&>))
import Entity.Arkham.Step
import Json hiding (Success)
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Types
import Network.HTTP.Types.Status qualified as Status

getApiV1ArkhamDecksR :: Handler [Entity ArkhamDeck]
getApiV1ArkhamDecksR = do
  userId <- getRequestUserId
  runDB $ select do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.userId ==. val userId
    pure decks

data CreateDeckPost = CreateDeckPost
  { deckId :: Text
  , deckName :: Text
  , deckUrl :: Maybe Text
  , deckList :: ArkhamDBDecklist
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype ValidateDeckPost = ValidateDeckPost
  { validateDeckList :: ArkhamDBDecklist
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data UpgradeDeckPost = UpgradeDeckPost
  { udpInvestigatorId :: InvestigatorId
  , udpDeckUrl :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON UpgradeDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"

newtype DeckError = UnimplementedCard CardCode
  deriving stock (Show, Eq, Generic)

instance ToJSON DeckError where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

toDeckErrors :: ArkhamDBDecklist -> [DeckError]
toDeckErrors decklist = flip mapMaybe cardCodes \cardCode ->
  maybe
    (Just $ UnimplementedCard cardCode)
    (const Nothing)
    (Map.lookup cardCode allPlayerCards)
 where
  cardCodes = Map.keys $ slots decklist

postApiV1ArkhamDecksR :: Handler (Entity ArkhamDeck)
postApiV1ArkhamDecksR = do
  userId <- getRequestUserId
  postData <- requireCheckJsonBody
  let deck = fromPostData userId postData
  case toDeckErrors (arkhamDeckList deck) of
    [] -> runDB $ insertEntity deck
    err -> sendStatusJSON status400 err

postApiV1ArkhamDecksValidateR :: Handler ()
postApiV1ArkhamDecksValidateR = do
  _ <- getRequestUserId
  decklist <- requireCheckJsonBody
  case toDeckErrors decklist of
    [] -> sendStatusJSON status200 ()
    err -> sendStatusJSON status400 err

putApiV1ArkhamGameDecksR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameDecksR gameId = do
  userId <- getRequestUserId
  ArkhamGame {..} <- runDB $ get404 gameId
  mLastStep <- runDB $ getBy (UniqueStep gameId arkhamGameStep)
  postData <- requireCheckJsonBody
  Entity playerId _ <- runDB $ getBy404 (UniquePlayer userId gameId)
  let Game {..} = arkhamGameCurrentData
  let investigatorId = udpInvestigatorId postData
  let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newQueue currentQueue
  genRef <- newIORef $ mkStdGen gameSeed
  runGameApp (GameApp gameRef queueRef genRef $ pure . const ()) do
    let question' = Map.delete (coerce playerId) gameQuestion
    unless (Map.null question') (push $ AskMap question')
    for_ (udpDeckUrl postData) \deckUrl -> do
      getDeckList deckUrl >>= either (error . show) \decklist -> do
        push
          $ if investigatorId /= decklist.investigator
            then case Map.lookup (toCardCode decklist.investigator) allInvestigatorCards of
              Nothing -> ReplaceInvestigator investigatorId decklist
              Just def ->
                if toCardCode investigatorId `elem` def.cardCodes
                  then UpgradeDecklist investigatorId decklist
                  else ReplaceInvestigator investigatorId decklist
            else UpgradeDecklist investigatorId decklist
    runMessages Nothing
  ge <- readIORef gameRef

  let diffDown = diff ge arkhamGameCurrentData
  updatedQueue <- readIORef (queueToRef queueRef)
  writeChannel <- (.channel) <$> getRoom gameId
  atomically
    $ writeTChan
      writeChannel
      (encode $ GameUpdate $ PublicGame gameId arkhamGameName mempty ge)
  now <- liftIO getCurrentTime
  runDB do
    replace gameId
      $ ArkhamGame
        arkhamGameName
        ge
        (arkhamGameStep + 1)
        arkhamGameMultiplayerVariant
        arkhamGameCreatedAt
        now
    insert_
      $ ArkhamStep
        gameId
        (Choice diffDown updatedQueue)
        (arkhamGameStep + 1)
        (ActionDiff $ view actionDiffL ge)

fromPostData :: UserId -> CreateDeckPost -> ArkhamDeck
fromPostData userId CreateDeckPost {..} = do
  ArkhamDeck
    { arkhamDeckUserId = userId
    , arkhamDeckUrl = deckUrl
    , arkhamDeckInvestigatorName = tshow $ investigator_name deckList
    , arkhamDeckName = deckName
    , arkhamDeckList = deckList
    }

getDeckList :: MonadIO m => Text -> m (Either String ArkhamDBDecklist)
getDeckList url = liftIO $ second (\d -> d {url = Just url}) . eitherDecode <$> simpleHttp (T.unpack url)

getApiV1ArkhamDeckR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
getApiV1ArkhamDeckR deckId = do
  userId <- getRequestUserId
  mDeck <- runDB $ selectOne do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.id ==. val deckId
    where_ $ decks.userId ==. val userId
    pure decks
  maybe notFound pure mDeck

deleteApiV1ArkhamDeckR :: ArkhamDeckId -> Handler ()
deleteApiV1ArkhamDeckR deckId = do
  userId <- getRequestUserId
  runDB $ delete do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.id ==. val deckId
    where_ $ decks.userId ==. val userId

newtype JSONError = JSONError {errorMsg :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

postApiV1ArkhamSyncDeckR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
postApiV1ArkhamSyncDeckR deckId = do
  userId <- getRequestUserId
  deck <- runDB $ get404 deckId
  unless (arkhamDeckUserId deck == userId) do
    sendStatusJSON
      Status.status400
      (JSONError "Deck does not belong to this user")
  edecklist <- maybe (pure $ Left "no deck url") getDeckList (arkhamDeckUrl deck)
  case edecklist of
    Right decklist -> do
      runDB $ update \d -> do
        set d [ArkhamDeckList =. val decklist]
        where_ $ d.id ==. val deckId
      pure $ Entity deckId $ deck {arkhamDeckList = decklist}
    Left _ -> sendStatusJSON Status.status400 (JSONError "Could not sync deck")

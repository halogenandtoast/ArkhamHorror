module Api.Handler.Arkham.Decks
  ( getApiV1ArkhamDecksR
  , postApiV1ArkhamDecksR
  , deleteApiV1ArkhamDeckR
  )
where

import Import hiding (delete, on, (==.))

import Database.Esqueleto
import Json
import Network.HTTP.Conduit (simpleHttp)
import Safe (fromJustNote)

getApiV1ArkhamDecksR :: Handler [Entity ArkhamDeck]
getApiV1ArkhamDecksR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ select $ from $ \decks -> do
    where_ (decks ^. ArkhamDeckUserId ==. val userId)
    pure decks

data CreateDeckPost = CreateDeckPost
  { deckId :: Text
  , deckName :: Text
  , deckUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

postApiV1ArkhamDecksR :: Handler (Entity ArkhamDeck)
postApiV1ArkhamDecksR = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  postData <- requireCheckJsonBody
  edeck <- fromPostData userId postData
  case edeck of
    Left err -> error err
    Right deck -> do
      deckId <- runDB (insert deck)
      pure $ Entity deckId deck

fromPostData
  :: (MonadIO m) => UserId -> CreateDeckPost -> m (Either String ArkhamDeck)
fromPostData userId CreateDeckPost {..} = do
  edecklist <- liftIO $ eitherDecode @ArkhamDBDecklist <$> simpleHttp
    (unpack deckUrl)
  pure $ case edecklist of
    Left err -> Left err
    Right decklist -> Right $ ArkhamDeck
      { arkhamDeckUserId = userId
      , arkhamDeckInvestigatorName = tshow $ investigator_name decklist
      , arkhamDeckName = deckName
      , arkhamDeckList = decklist
      }

deleteApiV1ArkhamDeckR :: ArkhamDeckId -> Handler ()
deleteApiV1ArkhamDeckR deckId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  void $ runDB $ do
    delete $ from $ \decks -> do
      where_ $ decks ^. persistIdField ==. val deckId
      where_ $ decks ^. ArkhamDeckUserId ==. val userId

module Api.Handler.Arkham.CustomCardSets (
  getApiV1ArkhamCustomCardSetsR,
  postApiV1ArkhamCustomCardSetsR,
  postApiV1ArkhamCustomCardSetsImportR,
  putApiV1ArkhamCustomCardSetR,
  deleteApiV1ArkhamCustomCardSetR,
) where

import Api.Handler.Arkham.CustomCards (ownedCardSet, saveCardInSet, stampSetName)
import Arkham.Card.CustomCard (CustomCard (..))
import Data.Aeson.Types (parseMaybe)
import Data.Text qualified as T
import Data.Time.Clock
import Import hiding ((==.))
import Import qualified as P
import Json hiding (Success)

{- | A set as the builder sees it: what it is called, where it came from, and how
much is in it. The count is what makes the sidebar useful, and is cheaper to
total here than to have the client tally a library it may not have loaded.
-}
data CustomCardSetResponse = CustomCardSetResponse
  { customCardSetResponseId :: ArkhamCustomCardSetId
  , customCardSetResponseName :: Text
  , customCardSetResponseSourceCode :: Maybe Text
  , customCardSetResponseCardCount :: Int
  , customCardSetResponseUpdatedAt :: UTCTime
  }
  deriving stock Generic

instance ToJSON CustomCardSetResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "customCardSetResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "customCardSetResponse"

data CustomCardSetImportResponse = CustomCardSetImportResponse
  { customCardSetImportResponseSet :: CustomCardSetResponse
  , customCardSetImportResponseCards :: [Entity ArkhamCustomCard]
  }
  deriving stock Generic

instance ToJSON CustomCardSetImportResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "customCardSetImportResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "customCardSetImportResponse"

newtype SetNamePost = SetNamePost {setNameName :: Text}
  deriving stock Generic

instance FromJSON SetNamePost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "setName"

data ImportSetPost = ImportSetPost
  { importSetName :: Text
  , importSetSourceCode :: Maybe Text
  , importSetCards :: [CustomCard]
  }
  deriving stock Generic

instance FromJSON ImportSetPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "importSet"

-- | A set with nothing in it is still a set; a set with no name is a mistake.
requireName :: Text -> Handler Text
requireName raw = do
  let name = T.strip raw
  when (T.null name) $ invalidArgs ["A set needs a name"]
  pure name

setResponse :: Entity ArkhamCustomCardSet -> Handler CustomCardSetResponse
setResponse (Entity setId row) = do
  cardCount <- runDB $ P.count [ArkhamCustomCardCustomCardSetId P.==. setId]
  pure
    $ CustomCardSetResponse
      { customCardSetResponseId = setId
      , customCardSetResponseName = arkhamCustomCardSetName row
      , customCardSetResponseSourceCode = arkhamCustomCardSetSourceCode row
      , customCardSetResponseCardCount = cardCount
      , customCardSetResponseUpdatedAt = arkhamCustomCardSetUpdatedAt row
      }

getApiV1ArkhamCustomCardSetsR :: Handler [CustomCardSetResponse]
getApiV1ArkhamCustomCardSetsR = do
  userId <- getRequestUserId
  sets <-
    runDB
      $ P.selectList [ArkhamCustomCardSetUserId P.==. userId] [P.Asc ArkhamCustomCardSetName]
  traverse setResponse sets

{- | Make a set. Asking twice for the same name gives you back the one you have
rather than refusing: the client is trying to end up with a set by that name,
and it already has one.
-}
postApiV1ArkhamCustomCardSetsR :: Handler CustomCardSetResponse
postApiV1ArkhamCustomCardSetsR = do
  userId <- getRequestUserId
  SetNamePost {setNameName} <- requireCheckJsonBody
  name <- requireName setNameName
  now <- liftIO getCurrentTime
  existing <- runDB $ P.getBy (UniqueUserCustomCardSetName userId name)
  case existing of
    Just found -> setResponse found
    Nothing -> do
      let row = ArkhamCustomCardSet userId name Nothing now now
      setId <- runDB $ P.insert row
      setResponse $ Entity setId row

{- | Rename, and carry the new name into the cards.

The copy each card holds is only a copy, but it is the one anything looking at a
def alone will read -- the deck overlay, the card picker, a card exported on its
own -- so leaving it behind would show the old name everywhere but here.
-}
putApiV1ArkhamCustomCardSetR :: ArkhamCustomCardSetId -> Handler CustomCardSetResponse
putApiV1ArkhamCustomCardSetR setId = do
  userId <- getRequestUserId
  SetNamePost {setNameName} <- requireCheckJsonBody
  name <- requireName setNameName
  row <- ownedCardSet userId setId
  now <- liftIO getCurrentTime

  clash <- runDB $ P.getBy (UniqueUserCustomCardSetName userId name)
  case clash of
    Just (Entity clashId _) | clashId /= setId -> invalidArgs ["You already have a set with that name"]
    _ -> pure ()

  cards <- runDB $ P.selectList [ArkhamCustomCardCustomCardSetId P.==. setId] []
  runDB do
    P.update setId [ArkhamCustomCardSetName P.=. name, ArkhamCustomCardSetUpdatedAt P.=. now]
    for_ cards \(Entity cardId card) ->
      for_ (parseMaybe parseJSON (arkhamCustomCardDef card)) \def ->
        P.update
          cardId
          [ ArkhamCustomCardDef P.=. toJSON (stampSetName name def)
          , ArkhamCustomCardUpdatedAt P.=. now
          ]

  setResponse
    $ Entity setId
    $ row {arkhamCustomCardSetName = name, arkhamCustomCardSetUpdatedAt = now}

{- | Deleting a set deletes what is in it.

That is the point of it being a set: importing a hundred and fifty cards and
changing your mind is one decision, not a hundred and fifty.
-}
deleteApiV1ArkhamCustomCardSetR :: ArkhamCustomCardSetId -> Handler ()
deleteApiV1ArkhamCustomCardSetR setId = do
  userId <- getRequestUserId
  void $ ownedCardSet userId setId
  runDB do
    -- The foreign key cascades, but the cards are deleted explicitly so this
    -- does not depend on the constraint being in place in a given database.
    P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. setId]
    P.delete setId

{- | Import a whole set at once, replacing one already here.

A set that has been imported before is matched on the pack id it came with, or
failing that its name, and its contents are replaced outright rather than merged
into: the file is the set as it now stands, so a card the file no longer has is
a card the set no longer has.
-}
postApiV1ArkhamCustomCardSetsImportR :: Handler CustomCardSetImportResponse
postApiV1ArkhamCustomCardSetsImportR = do
  userId <- getRequestUserId
  ImportSetPost {importSetName, importSetSourceCode, importSetCards} <- requireCheckJsonBody
  name <- requireName importSetName
  now <- liftIO getCurrentTime

  existing <- findSet userId name importSetSourceCode
  setId <- case existing of
    Just (Entity foundId _) -> do
      runDB do
        P.update
          foundId
          [ ArkhamCustomCardSetName P.=. name
          , ArkhamCustomCardSetSourceCode P.=. importSetSourceCode
          , ArkhamCustomCardSetUpdatedAt P.=. now
          ]
        P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. foundId]
      pure foundId
    Nothing -> runDB $ P.insert $ ArkhamCustomCardSet userId name importSetSourceCode now now

  cards <- traverse (saveCardInSet userId setId name now) importSetCards
  row <- runDB $ get404 setId
  response <- setResponse (Entity setId row)
  pure $ CustomCardSetImportResponse response cards

{- | The set an import means: the one from the same pack, else the one by that
name. A set built here has no pack id, so a pack cannot quietly adopt one unless
the names already match -- which is the case where replacing is what was meant.
-}
findSet :: UserId -> Text -> Maybe Text -> Handler (Maybe (Entity ArkhamCustomCardSet))
findSet userId name sourceCode = do
  bySource <- case sourceCode of
    Nothing -> pure Nothing
    Just code ->
      runDB
        $ P.selectFirst
          [ArkhamCustomCardSetUserId P.==. userId, ArkhamCustomCardSetSourceCode P.==. Just code]
          []
  case bySource of
    Just found -> pure $ Just found
    Nothing -> runDB $ P.getBy (UniqueUserCustomCardSetName userId name)

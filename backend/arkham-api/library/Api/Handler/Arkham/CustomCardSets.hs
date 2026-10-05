module Api.Handler.Arkham.CustomCardSets (
  getApiV1ArkhamCustomCardSetsR,
  postApiV1ArkhamCustomCardSetsR,
  postApiV1ArkhamCustomCardSetsImportR,
  putApiV1ArkhamCustomCardSetR,
  deleteApiV1ArkhamCustomCardSetR,
) where

import Api.Handler.Arkham.CustomCards (
  normalizeUrl,
  ownedCardSet,
  persistCard,
  prepareCardForSet,
  stampSetName,
  unsubscribeSet,
 )
import Arkham.Card.CustomCard (CustomCard (..))
import Auth.ApiKey qualified as ApiKey
import Data.Aeson.Types (parseMaybe)
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist qualified as DB
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
  , customCardSetResponseDescription :: Maybe Text
  {- ^ What the set is, in the author's own words. Seeds the marketplace
  listing when the set is published, and is what a page showing one set has
  to say about it beyond its name.
  -}
  , customCardSetResponseUrl :: Maybe Text
  {- ^ Where the set lives in the world: the post announcing it, the thread it
  is discussed in. Seeds the listing alongside the description, and is the one
  thing here that sends somebody somewhere else.
  -}
  , customCardSetResponseSourceCode :: Maybe Text
  , customCardSetResponseCardCount :: Int
  , customCardSetResponseUpdatedAt :: UTCTime
  , customCardSetResponsePublishedCardSetId :: Maybe ArkhamPublishedCardSetId
  {- ^ The marketplace listing this set follows, and where it stands with it.
  Absent once the set has been edited, which is what stops it updating.
  -}
  , customCardSetResponseSubscribedVersion :: Maybe Int
  , customCardSetResponseLatestVersion :: Maybe Int
  , customCardSetResponseSubmittedCardSetId :: Maybe ArkhamPublishedCardSetId
  {- ^ Your own marketplace listing for this set, if you have submitted it.
  Distinct from 'customCardSetResponsePublishedCardSetId', which is a listing
  of somebody else's that this set is a copy of.
  -}
  , customCardSetResponseSubmittedVersion :: Maybe Int
  {- ^ The version of the newest thing you submitted, and what was decided
  about it: @pending@, @approved@ or @denied@. The reason is only ever on a
  denial, and is what the author has to read to know what to change.
  -}
  , customCardSetResponseSubmissionStatus :: Maybe Text
  , customCardSetResponseSubmissionReason :: Maybe Text
  , customCardSetResponseApprovedVersion :: Maybe Int
  {- ^ The version of this set that is in the marketplace now, which is not
  necessarily the one you last submitted.
  -}
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

{- | What a create or an edit says about a set.

@description@ and @url@ are three-valued on purpose: absent leaves whatever is
there, null clears it, and a string sets it. A client that does not know about
either sends only a name, and @.:!@ is what keeps that from wiping one.
-}
data SetNamePost = SetNamePost
  { setNameName :: Text
  , setNameDescription :: Maybe (Maybe Text)
  , setNameUrl :: Maybe (Maybe Text)
  }

instance FromJSON SetNamePost where
  parseJSON = withObject "SetNamePost" \o ->
    SetNamePost <$> o .: "name" <*> o .:! "description" <*> o .:! "url"

{- | A description as it is stored: blank is nothing. Trimmed, so a field someone
typed a space into does not read as a description everywhere one is shown.
-}
normalizeDescription :: Maybe Text -> Maybe Text
normalizeDescription raw = do
  text <- T.strip <$> raw
  guard $ not (T.null text)
  pure text

data ImportSetPost = ImportSetPost
  { importSetName :: Text
  , importSetDescription :: Maybe Text
  , importSetUrl :: Maybe Text
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

{- | Where this set stands with the marketplace, if its owner has submitted it:
the listing, and the newest thing said about it.

Looked up by the listing pointing back at this set, which is how publishing again
finds the listing to add a version to.
-}
submissionFor
  :: ArkhamCustomCardSetId
  -> Handler (Maybe (Entity ArkhamPublishedCardSet), Maybe ArkhamCardSetSubmission)
submissionFor setId = do
  listing <-
    runDB
      $ P.selectFirst [ArkhamPublishedCardSetCustomCardSetId P.==. Just setId] []
  submission <- case listing of
    Nothing -> pure Nothing
    Just (Entity publishedId _) ->
      fmap (fmap entityVal)
        $ runDB
        $ P.selectFirst
          [ArkhamCardSetSubmissionPublishedCardSetId P.==. publishedId]
          [P.Desc ArkhamCardSetSubmissionVersion]
  pure (listing, submission)

setResponse :: Entity ArkhamCustomCardSet -> Handler CustomCardSetResponse
setResponse (Entity setId row) = do
  cardCount <- runDB $ P.count [ArkhamCustomCardCustomCardSetId P.==. setId]
  subscription <- runDB $ P.getBy (UniqueCardSetSubscription setId)
  -- What a subscriber can update to is the newest *approved* version: an author's
  -- unreviewed submission is not an update, and is not importable.
  latest <- case subscription of
    Nothing -> pure Nothing
    Just (Entity _ sub) ->
      fmap (arkhamPublishedCardSetApprovedVersion =<<)
        $ runDB
        $ DB.get (arkhamCardSetSubscriptionPublishedCardSetId sub)
  (listing, submission) <- submissionFor setId
  pure
    $ CustomCardSetResponse
      { customCardSetResponseId = setId
      , customCardSetResponseName = arkhamCustomCardSetName row
      , customCardSetResponseDescription = arkhamCustomCardSetDescription row
      , customCardSetResponseUrl = arkhamCustomCardSetUrl row
      , customCardSetResponseSourceCode = arkhamCustomCardSetSourceCode row
      , customCardSetResponseCardCount = cardCount
      , customCardSetResponseUpdatedAt = arkhamCustomCardSetUpdatedAt row
      , customCardSetResponsePublishedCardSetId =
          arkhamCardSetSubscriptionPublishedCardSetId . entityVal <$> subscription
      , customCardSetResponseSubscribedVersion =
          arkhamCardSetSubscriptionVersion . entityVal <$> subscription
      , customCardSetResponseLatestVersion = latest
      , customCardSetResponseSubmittedCardSetId = entityKey <$> listing
      , customCardSetResponseSubmittedVersion =
          arkhamCardSetSubmissionVersion <$> submission
      , customCardSetResponseSubmissionStatus =
          arkhamCardSetSubmissionStatus <$> submission
      , customCardSetResponseSubmissionReason =
          arkhamCardSetSubmissionReason =<< submission
      , customCardSetResponseApprovedVersion =
          arkhamPublishedCardSetApprovedVersion . entityVal =<< listing
      }

getApiV1ArkhamCustomCardSetsR :: Handler [CustomCardSetResponse]
getApiV1ArkhamCustomCardSetsR = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsRead]
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
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  SetNamePost {setNameName, setNameDescription, setNameUrl} <- requireCheckJsonBody
  name <- requireName setNameName
  url <- normalizeUrl (join setNameUrl)
  now <- liftIO getCurrentTime
  existing <- runDB $ P.getBy (UniqueUserCustomCardSetName userId name)
  case existing of
    Just found -> setResponse found
    Nothing -> do
      let description = normalizeDescription (join setNameDescription)
          row = ArkhamCustomCardSet userId name description url Nothing now now
      setId <- runDB $ P.insert row
      setResponse $ Entity setId row

{- | Rename or redescribe, carrying a new name into the cards.

The copy of the name each card holds is only a copy, but it is the one that
travels with a card out of here -- an export, and from there someone else's
account -- so leaving it behind would hand out cards still naming the old set.
Neither the description nor the link is stamped onto the cards: they describe
the set, and a card on its own is not the set.

Renaming stops the set following a published one -- what is in it is then not
what was published. Rewriting only the description or the link does not: nothing
about the cards has changed, and taking someone's updates away for editing a
blurb would be a trap.
-}
putApiV1ArkhamCustomCardSetR :: ArkhamCustomCardSetId -> Handler CustomCardSetResponse
putApiV1ArkhamCustomCardSetR setId = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  SetNamePost {setNameName, setNameDescription, setNameUrl} <- requireCheckJsonBody
  name <- requireName setNameName
  row <- ownedCardSet userId setId
  now <- liftIO getCurrentTime

  clash <- runDB $ P.getBy (UniqueUserCustomCardSetName userId name)
  case clash of
    Just (Entity clashId _) | clashId /= setId -> invalidArgs ["You already have a set with that name"]
    _ -> pure ()

  let renamed = name /= arkhamCustomCardSetName row
      description = case setNameDescription of
        Nothing -> arkhamCustomCardSetDescription row
        Just given -> normalizeDescription given
  url <- case setNameUrl of
    Nothing -> pure $ arkhamCustomCardSetUrl row
    Just given -> normalizeUrl given

  cards <- runDB $ P.selectList [ArkhamCustomCardCustomCardSetId P.==. setId] []
  runDB do
    when renamed $ unsubscribeSet setId
    P.update
      setId
      [ ArkhamCustomCardSetName P.=. name
      , ArkhamCustomCardSetDescription P.=. description
      , ArkhamCustomCardSetUrl P.=. url
      , ArkhamCustomCardSetUpdatedAt P.=. now
      ]
    when renamed
      $ for_ cards \(Entity cardId card) ->
        for_ (parseMaybe parseJSON (arkhamCustomCardDef card)) \def ->
          P.update
            cardId
            [ ArkhamCustomCardDef P.=. toJSON (stampSetName name def)
            , ArkhamCustomCardUpdatedAt P.=. now
            ]

  setResponse
    $ Entity setId
    $ row
      { arkhamCustomCardSetName = name
      , arkhamCustomCardSetDescription = description
      , arkhamCustomCardSetUrl = url
      , arkhamCustomCardSetUpdatedAt = now
      }

{- | Deleting a set deletes what is in it.

That is the point of it being a set: importing a hundred and fifty cards and
changing your mind is one decision, not a hundred and fifty.
-}
deleteApiV1ArkhamCustomCardSetR :: ArkhamCustomCardSetId -> Handler ()
deleteApiV1ArkhamCustomCardSetR setId = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
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
  ImportSetPost
    { importSetName
    , importSetDescription
    , importSetUrl
    , importSetSourceCode
    , importSetCards
    } <-
    requireCheckJsonBody
  name <- requireName importSetName
  url <- normalizeUrl importSetUrl
  let description = normalizeDescription importSetDescription
  now <- liftIO getCurrentTime

  existing <- findSet userId name importSetSourceCode

  {- A set matched by its pack id can be carrying a name some *other* set of
  yours already holds -- the pack was renamed to something you had used. Renaming
  it into that name breaks the unique index, so say what is wrong rather than
  letting the constraint answer with a 500. -}
  clash <- runDB $ P.getBy (UniqueUserCustomCardSetName userId name)
  case (existing, clash) of
    (Just (Entity foundId _), Just (Entity clashId _))
      | clashId /= foundId ->
          invalidArgs ["You already have a different set called \"" <> name <> "\""]
    _ -> pure ()

  {- Art is uploaded before anything is written. The write below empties the set
  before refilling it, so a card that cannot be stored -- an image the art host
  refuses, a code that is not a custom card's -- has to fail here, while the set
  it is replacing is still whole. -}
  prepared <- traverse (prepareCardForSet userId name) importSetCards

  (setId, cards) <- runDB do
    setId <- case existing of
      Just (Entity foundId found) -> do
        P.update
          foundId
          [ ArkhamCustomCardSetName P.=. name
          , -- A file that carries no description leaves the one already here:
            -- a set exported before descriptions existed should not blank the
            -- blurb its owner has since written.
            ArkhamCustomCardSetDescription
              P.=. (description <|> arkhamCustomCardSetDescription found)
          , ArkhamCustomCardSetUrl P.=. (url <|> arkhamCustomCardSetUrl found)
          , ArkhamCustomCardSetSourceCode P.=. importSetSourceCode
          , ArkhamCustomCardSetUpdatedAt P.=. now
          ]
        P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. foundId]
        pure foundId
      Nothing ->
        P.insert $ ArkhamCustomCardSet userId name description url importSetSourceCode now now
    -- Emptying and refilling in one transaction: an import that dies partway
    -- leaves the set as it was rather than as half of what replaced it.
    (setId,) <$> traverse (persistCard userId setId now) prepared

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

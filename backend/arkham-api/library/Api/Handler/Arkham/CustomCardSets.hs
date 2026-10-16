module Api.Handler.Arkham.CustomCardSets (
  getApiV1ArkhamCustomCardSetsR,
  postApiV1ArkhamCustomCardSetUnlockR,
  postApiV1ArkhamCustomCardSetsR,
  postApiV1ArkhamCustomCardSetsImportR,
  putApiV1ArkhamCustomCardSetR,
  deleteApiV1ArkhamCustomCardSetR,
) where

import Api.Handler.Arkham.CustomCards (
  normalizeUrl,
  ownedCardSet,
  persistCard,
  portableArt,
  prepareCardForSet,
  requireUnlocked,
  setDeclarationsFromCards,
  stampSet,
 )
import Arkham.Card.CustomCard (CustomCard (..))
import Arkham.Custom.SetDefs qualified as SetDefs
import Auth.ApiKey qualified as ApiKey
import Data.Aeson.Types (parseMaybe)
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable (for)
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
  , customCardSetResponseInvestigatorExpansion :: Bool
  {- ^ Whether the set is an investigator expansion: a box, rather than a
  handful of cards. The three fields below are only asked about when it is.
  -}
  , customCardSetResponseIcon :: Maybe Text
  {- ^ The expansion symbol, which the marketplace files the set under. Shown
  only; nothing in a game reads it.
  -}
  , customCardSetResponseTokens :: Value
  {- ^ The markers the set's cards place on themselves, as
  @[{slug, name, image}]@. Not chaos tokens -- those belong to a scenario.
  -}
  , customCardSetResponseKeywords :: Value
  {- ^ The keywords the set's cards print, as
  @[{name, description, token, shape}]@.
  -}
  , customCardSetResponseCardCount :: Int
  , customCardSetResponseUpdatedAt :: UTCTime
  , customCardSetResponseLocked :: Bool
  {- ^ Whether this set follows somebody else's -- a marketplace listing, or a
  link somebody sent you -- and so cannot be edited. The three ways a set is
  changed (its cards, its details, a file imported over it) all refuse, and the
  way out is "Add to my editor", which makes it an ordinary set of yours.
  -}
  , customCardSetResponsePublishedCardSetId :: Maybe ArkhamPublishedCardSetId
  {- ^ The marketplace listing this set follows, and where it stands with it.
  Absent on a copy that was imported rather than subscribed, and on one since
  taken into the editor: both are yours, and a listing has nothing more to say
  about either.
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
  , customCardSetResponseShareToken :: Maybe Text
  {- ^ The secret in this set's share link, if it is being shared with anyone.
  Carried on the set rather than fetched when the share panel is opened, so the
  row can say that the set is shared and hand over the link without a request
  per set.
  -}
  , customCardSetResponseShareCount :: Int
  {- ^ How many people have opened the link and not been removed since. What
  makes "shared" worth a chip: a link nobody has used says something different
  from one three people are playing with.
  -}
  , customCardSetResponseSharedToken :: Maybe Text
  {- ^ The share link this set follows, for a set somebody handed you.
  The other direction from 'customCardSetResponseShareToken', which is a link
  of your own: this is the way back to the page that can take the author's
  newest cards, and the only way back, since the url arrived in a chat window.

  Absent once the author stops sharing or removes you, because there is then
  nothing at the other end of it.
  -}
  , customCardSetResponseSharedBy :: Maybe Text
  {- ^ Who shared it. The one thing a copy taken through a link has to say for
  itself that nothing else on the row can: a set with no author named is a set
  that arrived from nowhere.
  -}
  , customCardSetResponseSharedBehind :: Bool
  {- ^ The author has changed that set since you took it. The same comparison
  their own page makes from the other side -- there is no "up to date" flag for
  an edit of theirs to go stale against.
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

Every field but the name is three-valued on purpose: absent leaves whatever is
there, null clears it, and a value sets it. A client that does not know about
one of them sends only a name, and @.:!@ is what keeps that from wiping the
rest.
-}
data SetNamePost = SetNamePost
  { setNameName :: Text
  , setNameDescription :: Maybe (Maybe Text)
  , setNameUrl :: Maybe (Maybe Text)
  , setNameInvestigatorExpansion :: Maybe Bool
  , setNameIcon :: Maybe (Maybe Text)
  , setNameTokens :: Maybe Value
  , setNameKeywords :: Maybe Value
  }

instance FromJSON SetNamePost where
  parseJSON = withObject "SetNamePost" \o ->
    SetNamePost
      <$> o
      .: "name"
      <*> o
      .:! "description"
      <*> o
      .:! "url"
      <*> o
      .:! "investigatorExpansion"
      <*> o
      .:! "icon"
      <*> o
      .:! "tokens"
      <*> o
      .:! "keywords"

{- | A description as it is stored: blank is nothing. Trimmed, so a field someone
typed a space into does not read as a description everywhere one is shown.
-}
normalizeDescription :: Maybe Text -> Maybe Text
normalizeDescription raw = do
  text <- T.strip <$> raw
  guard $ not (T.null text)
  pure text

{- | A set's tokens as they are stored: checked against the type, trimmed of
anything with no slug, and with any image that arrived inline uploaded and any
image this host cannot serve dropped ('portableArt').

Checked rather than stored as given because these are stamped onto every card
in the set and read by the engine from there. A token the engine cannot decode
is a card that places nothing, silently -- the same failure the whole card
pipeline is built to refuse, and the reason this is a 400 rather than a shrug.
-}
normalizeTokens :: UserId -> Value -> Handler Value
normalizeTokens userId raw = case parseMaybe parseJSON raw of
  Nothing -> invalidArgs ["A set's tokens have to be a list of {slug, name, image}"]
  Just (tokens :: [SetDefs.CustomSetToken]) -> do
    let named = [t | t <- tokens, not (T.null (T.strip (SetDefs.customSetTokenSlug t)))]
    hosted <- for named \t -> do
      image <- join <$> traverse (portableArt userId) (SetDefs.customSetTokenImage t)
      pure t {SetDefs.customSetTokenImage = image}
    pure $ toJSON hosted

{- | A set's keywords as they are stored. Checked for the same reason the tokens
are: a card's @_keywords@ names one of these, and a keyword that will not decode
is a keyword no card can print.
-}
normalizeKeywords :: Value -> Handler Value
normalizeKeywords raw = case parseMaybe parseJSON raw of
  Nothing ->
    invalidArgs ["A set's keywords have to be a list of {name, description, token, shape}"]
  Just (keywords :: [SetDefs.CustomSetKeyword]) ->
    pure
      $ toJSON [k | k <- keywords, not (T.null (T.strip (SetDefs.customSetKeywordName k)))]

{- | An icon as it is stored: an image uploaded here, or nothing.

An inline image is uploaded like a card's art; a URL is kept as given, which is
what an export that could not read its own icon comes back as -- unless it is a
path into the library it came from, which this host cannot serve ('portableArt').
-}
normalizeIcon :: UserId -> Maybe Text -> Handler (Maybe Text)
normalizeIcon userId raw = case T.strip <$> raw of
  Nothing -> pure Nothing
  Just text | T.null text -> pure Nothing
  Just text -> portableArt userId text

data ImportSetPost = ImportSetPost
  { importSetName :: Text
  , importSetDescription :: Maybe Text
  , importSetUrl :: Maybe Text
  , importSetSourceCode :: Maybe Text
  , importSetInvestigatorExpansion :: Maybe Bool
  , importSetIcon :: Maybe Text
  , importSetTokens :: Maybe Value
  , importSetKeywords :: Maybe Value
  {- ^ What the set declares. Optional, because a file written before
  expansions existed has none and a single card pasted on its own is not a set
  at all -- in both cases the cards are asked instead, since each carries a
  copy (see 'setDeclarationsFromCards').
  -}
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

{- | The share link this set follows, and whether the author has moved on since.

Found by the access row pointing at this set, which is what records that the copy
was taken through a link at all. Only a row that is still subscribed and not
revoked: a copy that was imported, or taken into the editor, is an ordinary set
of the caller's and follows nothing, and a dead link is nothing to send anyone
back to.
-}
sharedSource :: ArkhamCustomCardSetId -> Handler SharedFrom
sharedSource setId = do
  access <-
    runDB
      $ P.selectFirst
        [ ArkhamCardSetShareAccessCustomCardSetId P.==. Just setId
        , ArkhamCardSetShareAccessSubscribed P.==. True
        , ArkhamCardSetShareAccessRevokedAt P.==. Nothing
        ]
        []
  case access of
    Nothing -> pure $ SharedFrom Nothing Nothing False
    Just (Entity _ row) -> do
      share <- runDB $ DB.get (arkhamCardSetShareAccessCardSetShareId row)
      source <- maybe (pure Nothing) (runDB . DB.get . arkhamCardSetShareCustomCardSetId) share
      author <- maybe (pure Nothing) (runDB . DB.get . arkhamCustomCardSetUserId) source
      let behind = fromMaybe False do
            at <- arkhamCardSetShareAccessTakenAt row
            set <- source
            pure $ arkhamCustomCardSetUpdatedAt set > at
      pure
        $ SharedFrom
          (arkhamCardSetShareToken <$> share)
          (userUsername <$> author)
          behind

{- | Where a set in somebody's collection came from, when it came from a link.

Everything null and false for a set of their own, which is almost all of them.
-}
data SharedFrom = SharedFrom
  { sharedFromToken :: Maybe Text
  , sharedFromAuthor :: Maybe Text
  , sharedFromBehind :: Bool
  }

setResponse :: Entity ArkhamCustomCardSet -> Handler CustomCardSetResponse
setResponse (Entity setId row) = do
  cardCount <- runDB $ P.count [ArkhamCustomCardCustomCardSetId P.==. setId]
  {- Only a subscription that is still followed. The row outlives that -- it is
  also "this set of mine is my copy of that listing", which is how taking it
  again replaces the copy rather than making a second one -- but a copy taken
  into the editor is an ordinary set of yours, and nothing here should still be
  reporting a version it is on or offering it an update. -}
  subscription <-
    runDB
      $ P.selectFirst
        [ ArkhamCardSetSubscriptionCustomCardSetId P.==. setId
        , ArkhamCardSetSubscriptionSubscribed P.==. True
        ]
        []
  -- What a subscriber can update to is the newest *approved* version: an author's
  -- unreviewed submission is not an update, and is not importable.
  latest <- case subscription of
    Nothing -> pure Nothing
    Just (Entity _ sub) ->
      fmap (arkhamPublishedCardSetApprovedVersion =<<)
        $ runDB
        $ DB.get (arkhamCardSetSubscriptionPublishedCardSetId sub)
  (listing, submission) <- submissionFor setId
  share <- runDB $ P.getBy (UniqueCardSetShare setId)
  shared <- case share of
    Nothing -> pure 0
    Just (Entity shareId _) ->
      runDB
        $ P.count
          [ ArkhamCardSetShareAccessCardSetShareId P.==. shareId
          , ArkhamCardSetShareAccessRevokedAt P.==. Nothing
          ]
  sharedFrom <- sharedSource setId
  let locked = isJust subscription || isJust (sharedFromToken sharedFrom)
  pure
    $ CustomCardSetResponse
      { customCardSetResponseId = setId
      , customCardSetResponseName = arkhamCustomCardSetName row
      , customCardSetResponseDescription = arkhamCustomCardSetDescription row
      , customCardSetResponseUrl = arkhamCustomCardSetUrl row
      , customCardSetResponseSourceCode = arkhamCustomCardSetSourceCode row
      , customCardSetResponseInvestigatorExpansion = arkhamCustomCardSetInvestigatorExpansion row
      , customCardSetResponseIcon = arkhamCustomCardSetIcon row
      , customCardSetResponseTokens = arkhamCustomCardSetTokens row
      , customCardSetResponseKeywords = arkhamCustomCardSetKeywords row
      , customCardSetResponseCardCount = cardCount
      , customCardSetResponseUpdatedAt = arkhamCustomCardSetUpdatedAt row
      , customCardSetResponseLocked = locked
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
      , customCardSetResponseShareToken = arkhamCardSetShareToken . entityVal <$> share
      , customCardSetResponseShareCount = shared
      , customCardSetResponseSharedToken = sharedFromToken sharedFrom
      , customCardSetResponseSharedBy = sharedFromAuthor sharedFrom
      , customCardSetResponseSharedBehind = sharedFromBehind sharedFrom
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
  post <- requireCheckJsonBody
  name <- requireName (setNameName post)
  url <- normalizeUrl (join (setNameUrl post))
  now <- liftIO getCurrentTime
  existing <- runDB $ P.getBy (UniqueUserCustomCardSetName userId name)
  case existing of
    Just found -> setResponse found
    Nothing -> do
      let description = normalizeDescription (join (setNameDescription post))
      icon <- normalizeIcon userId (join (setNameIcon post))
      tokens <- maybe (pure emptyDeclaration) (normalizeTokens userId) (setNameTokens post)
      keywords <- maybe (pure emptyDeclaration) normalizeKeywords (setNameKeywords post)
      let row =
            ArkhamCustomCardSet
              userId
              name
              description
              url
              Nothing
              (fromMaybe False (setNameInvestigatorExpansion post))
              icon
              tokens
              keywords
              now
              now
      setId <- runDB $ P.insert row
      setResponse $ Entity setId row

-- | What a set that declares no tokens and no keywords holds in those columns.
emptyDeclaration :: Value
emptyDeclaration = toJSON ([] :: [Value])

{- | Rename or redescribe, carrying what the set declares into the cards.

The name, the tokens and the keywords each card holds are only copies, but they
are the ones that travel with a card out of here -- an export, and from there
someone else's account, and a running game, where the def is all there is -- so
leaving them behind would hand out cards naming the old set, or printing a
keyword with nothing to say what it means. Neither the description nor the link
nor the icon is stamped onto the cards: they describe the set, and a card on its
own is not the set.

A set that follows somebody else's is refused outright, blurb and all: it is
their set, and what it says about itself is theirs to write. "Add to my editor"
is the way to make it yours.
-}
putApiV1ArkhamCustomCardSetR :: ArkhamCustomCardSetId -> Handler CustomCardSetResponse
putApiV1ArkhamCustomCardSetR setId = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  post <- requireCheckJsonBody
  name <- requireName (setNameName post)
  row <- ownedCardSet userId setId
  requireUnlocked setId
  now <- liftIO getCurrentTime

  clash <- runDB $ P.getBy (UniqueUserCustomCardSetName userId name)
  case clash of
    Just (Entity clashId _) | clashId /= setId -> invalidArgs ["You already have a set with that name"]
    _ -> pure ()

  let description = case setNameDescription post of
        Nothing -> arkhamCustomCardSetDescription row
        Just given -> normalizeDescription given
      expansion =
        fromMaybe (arkhamCustomCardSetInvestigatorExpansion row) (setNameInvestigatorExpansion post)
  url <- case setNameUrl post of
    Nothing -> pure $ arkhamCustomCardSetUrl row
    Just given -> normalizeUrl given
  icon <- case setNameIcon post of
    Nothing -> pure $ arkhamCustomCardSetIcon row
    Just given -> normalizeIcon userId given
  tokens <- case setNameTokens post of
    Nothing -> pure $ arkhamCustomCardSetTokens row
    Just given -> normalizeTokens userId given
  keywords <- case setNameKeywords post of
    Nothing -> pure $ arkhamCustomCardSetKeywords row
    Just given -> normalizeKeywords given

  let updated =
        row
          { arkhamCustomCardSetName = name
          , arkhamCustomCardSetDescription = description
          , arkhamCustomCardSetUrl = url
          , arkhamCustomCardSetInvestigatorExpansion = expansion
          , arkhamCustomCardSetIcon = icon
          , arkhamCustomCardSetTokens = tokens
          , arkhamCustomCardSetKeywords = keywords
          , arkhamCustomCardSetUpdatedAt = now
          }
      -- Everything the cards carry a copy of. Any of it changing is the cards
      -- changing, so it is what has to be re-stamped onto them.
      restamp =
        name
          /= arkhamCustomCardSetName row
          || tokens
          /= arkhamCustomCardSetTokens row
          || keywords
          /= arkhamCustomCardSetKeywords row

  cards <- runDB $ P.selectList [ArkhamCustomCardCustomCardSetId P.==. setId] []
  runDB do
    P.update
      setId
      [ ArkhamCustomCardSetName P.=. name
      , ArkhamCustomCardSetDescription P.=. description
      , ArkhamCustomCardSetUrl P.=. url
      , ArkhamCustomCardSetInvestigatorExpansion P.=. expansion
      , ArkhamCustomCardSetIcon P.=. icon
      , ArkhamCustomCardSetTokens P.=. tokens
      , ArkhamCustomCardSetKeywords P.=. keywords
      , ArkhamCustomCardSetUpdatedAt P.=. now
      ]
    when restamp
      $ for_ cards \(Entity cardId card) ->
        for_ (parseMaybe parseJSON (arkhamCustomCardDef card)) \def ->
          P.update
            cardId
            [ ArkhamCustomCardDef P.=. toJSON (stampSet updated def)
            , ArkhamCustomCardUpdatedAt P.=. now
            ]

  setResponse $ Entity setId updated

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

{- | Take a followed set into your editor: stop following it, and keep the cards.

The way out of a locked copy of a /published/ set, and the reason locking one is
not a trap. What you have becomes an ordinary set of yours -- editable,
publishable, shareable -- and stops hearing from the listing. The cards do not
move and nothing is fetched: they are already here, and the only thing that
changes is that this copy no longer claims to be what was published.

The subscription row stays, with the flag off, because it is also "this set of
mine is my copy of that listing" -- so subscribing again replaces this copy
rather than leaving you with two under a numbered name, which a card code being
unique per account would not let you keep anyway.

A copy taken from a share link is refused. That set is unpublished: it was handed
to a few people to try, not given away, and an editable copy of it is a copy that
can be exported and passed on. Nothing here is the author's loss to take -- the
only ways out of a shared copy are to leave it as it is or to delete it, and the
author is the only person who can hand the cards on.

Asking for a set that follows nothing is already the outcome, so it is a no-op
rather than an error: the client is trying to end up with a set it can edit.
-}
postApiV1ArkhamCustomCardSetUnlockR :: ArkhamCustomCardSetId -> Handler CustomCardSetResponse
postApiV1ArkhamCustomCardSetUnlockR setId = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  row <- ownedCardSet userId setId
  sharedFrom <- sharedSource setId
  when (isJust (sharedFromToken sharedFrom))
    $ permissionDenied
      "That set was shared with you rather than published. It stays the author's."
  now <- liftIO getCurrentTime
  runDB
    $ P.updateWhere
      [ArkhamCardSetSubscriptionCustomCardSetId P.==. setId]
      [ArkhamCardSetSubscriptionSubscribed P.=. False, ArkhamCardSetSubscriptionUpdatedAt P.=. now]
  setResponse $ Entity setId row

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
    , importSetInvestigatorExpansion
    , importSetIcon
    , importSetTokens
    , importSetKeywords
    , importSetCards
    } <-
    requireCheckJsonBody
  name <- requireName importSetName
  url <- normalizeUrl importSetUrl
  let description = normalizeDescription importSetDescription
      (stampedTokens, stampedKeywords) = setDeclarationsFromCards importSetCards
  icon <- normalizeIcon userId importSetIcon
  tokens <- normalizeTokens userId (fromMaybe stampedTokens importSetTokens)
  keywords <- normalizeKeywords (fromMaybe stampedKeywords importSetKeywords)
  let expansion =
        fromMaybe
          -- A file that did not say is an expansion if it brought any of the
          -- furniture only an expansion has.
          (tokens /= emptyDeclaration || keywords /= emptyDeclaration)
          importSetInvestigatorExpansion
  now <- liftIO getCurrentTime

  existing <- findSet userId name importSetSourceCode
  -- A file replaces a set outright, which is exactly what must not happen to a
  -- copy that is following somebody else's.
  for_ existing \(Entity foundId _) -> requireUnlocked foundId

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
  let incoming =
        ArkhamCustomCardSet
          userId
          name
          description
          url
          importSetSourceCode
          expansion
          icon
          tokens
          keywords
          now
          now
  prepared <- traverse (prepareCardForSet userId incoming) importSetCards

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
          , ArkhamCustomCardSetInvestigatorExpansion P.=. expansion
          , ArkhamCustomCardSetIcon P.=. (icon <|> arkhamCustomCardSetIcon found)
          , ArkhamCustomCardSetTokens P.=. tokens
          , ArkhamCustomCardSetKeywords P.=. keywords
          , ArkhamCustomCardSetUpdatedAt P.=. now
          ]
        P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. foundId]
        pure foundId
      Nothing -> P.insert incoming
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

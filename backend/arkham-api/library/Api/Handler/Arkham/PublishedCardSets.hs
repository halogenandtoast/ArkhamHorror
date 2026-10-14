{- | The custom card marketplace: sets an author has submitted, and other
people's copies following them.

Publishing snapshots the set's cards into a new version and submits that version
for review. Nothing is listed or importable until an admin approves it, and each
version is reviewed on its own -- so a set that passed once cannot be quietly
swapped for something else. 'Api.Handler.Arkham.CardSetSubmissions' is the other
half of that: the queue, and the decision.

An admin publishing skips the queue: they are the person review would have waited
for, so the version is recorded as approved by them and listed at once. It is the
same path and the same records -- only the decision is already made.

Taking a set is two different things, and the page offers both. Subscribing
records which version you landed on and leaves the copy locked, so it stays what
was published and taking it again brings it up to date. Importing hands you an
ordinary set of your own: editable, frozen where it was, hearing nothing more
from the listing. A card code is unique per account, so nobody holds both -- one
always replaces the other -- and "Add to my editor" turns the first into the
second without fetching anything.

Either way the same import a file import does runs, and the same row records
which set of yours is your copy of the listing; only the flag on it differs.
-}
module Api.Handler.Arkham.PublishedCardSets (
  getApiV1ArkhamPublishedCardSetsR,
  getApiV1ArkhamPublishedCardSetR,
  putApiV1ArkhamPublishedCardSetR,
  deleteApiV1ArkhamPublishedCardSetR,
  postApiV1ArkhamPublishedCardSetSubscribeR,
  postApiV1ArkhamPublishedCardSetImportR,
  postApiV1ArkhamCustomCardSetPublishR,
  postApiV1ArkhamCustomCardSetSyncR,
  postApiV1ArkhamPublishedCardSetLikeR,
  deleteApiV1ArkhamPublishedCardSetLikeR,
  listingFor,
  versionCards,
) where

import Api.Handler.Arkham.CustomCards (
  Source (..),
  followOnly,
  freeName,
  myCopyOfListing,
  myCopyOfSet,
  normalizeUrl,
  ownedCardSet,
  persistCard,
  prepareCardForSet,
  printedOrder,
  requireUnlocked,
  setDeclarationsFromCards,
 )
import Arkham.Card.CustomCard (CustomCard (..))
import Data.Aeson.Types (parseEither)
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable (for)
import Database.Persist qualified as DB
import Database.Persist.Sql (SqlPersistT)
import Import hiding ((==.))
import Import qualified as P
import Json hiding (Success)
import Network.HTTP.Types.Status qualified as Status

-- | A marketplace row: what it is, who wrote it, and where you stand with it.
data PublishedCardSetResponse = PublishedCardSetResponse
  { publishedCardSetResponseId :: ArkhamPublishedCardSetId
  , publishedCardSetResponseName :: Text
  , publishedCardSetResponseDescription :: Maybe Text
  {- ^ What the set is. The listing's own, not the version's: the author can
  rewrite it without republishing and without anyone reviewing it again,
  which is the whole reason it does not live on the snapshot.
  -}
  , publishedCardSetResponseUrl :: Maybe Text
  {- ^ Where the set lives in the world -- the post announcing it, the thread
  it is discussed in -- for the one thing a listing cannot say for itself.
  The listing's own, like the description, and rewritable the same way.
  -}
  , publishedCardSetResponseIcon :: Maybe Text
  {- ^ The expansion symbol, for drawing the listing. A copy taken when the set
  was published, so a card is never loaded to put a symbol beside a name.
  -}
  , publishedCardSetResponseInvestigatorExpansion :: Bool
  -- ^ Whether this is an investigator expansion rather than a handful of cards.
  , publishedCardSetResponseAuthor :: Text
  , publishedCardSetResponseMine :: Bool
  , publishedCardSetResponseOfficial :: Bool
  {- ^ Whether the author is an admin, which is what the marketplace calls
  officially supported: an admin publishing is the project putting a set up
  rather than passing one.

  Read off the account every time rather than stamped on the listing when it
  was published. It costs nothing -- the author is already fetched for their
  name -- and a set stops claiming support when whoever was maintaining it
  stops being able to.
  -}
  , publishedCardSetResponseLatestVersion :: Int
  {- ^ The newest version a reviewer has approved: the one being described
  below, and the one an import gets. Zero for a set of your own that has
  never passed review, which is the only way an unapproved set is listed.
  -}
  , publishedCardSetResponseReviewStatus :: Text
  {- ^ Where the newest submission stands: @approved@, @pending@, @denied@, or
  @none@ for a listing with no submission at all. Only ever anything but
  @approved@ on your own listing, since nobody else's is shown to you until
  it has passed.
  -}
  , publishedCardSetResponsePendingVersion :: Maybe Int
  -- ^ A version of yours waiting to be reviewed, if one is.
  , publishedCardSetResponseDenialReason :: Maybe Text
  -- ^ Why the last submission was turned down, for the author to read.
  , publishedCardSetResponseCardCount :: Int
  , publishedCardSetResponseNote :: Maybe Text
  , publishedCardSetResponsePreview :: [CustomCard]
  {- ^ The first few cards, so the listing can show what is in the set rather
  than only how much. Capped because a listing is a row, not the set.
  -}
  , publishedCardSetResponseLikes :: Int
  , publishedCardSetResponseLiked :: Bool
  -- ^ Whether you are one of them, so the button knows which way it points.
  , publishedCardSetResponseUpdatedAt :: UTCTime
  , publishedCardSetResponseSubscribedVersion :: Maybe Int
  -- ^ The version your own copy is following, if you have one.
  , publishedCardSetResponseVersions :: [PublishedVersionSummary]
  {- ^ Every version of your own listing, newest first, with what was decided
    about each. Empty on anybody else's: what they submitted and what was turned
    down is the author's business, and only the approved one is theirs to see.
  -}
  }
  deriving stock Generic

{- | One version of a listing, as its author's own history of it reads: what they
said about it, and what came of it.

The cards are left out. This is a list of what has happened to a set, and the
cards of a version are a request of their own.
-}
data PublishedVersionSummary = PublishedVersionSummary
  { publishedVersionSummaryVersion :: Int
  , publishedVersionSummaryNote :: Maybe Text
  , publishedVersionSummaryStatus :: Text
  {- ^ @pending@, @approved@, @denied@, or @none@ for a version with no
  submission against it -- which should not happen, but says so rather than
  claiming one of the three.
  -}
  , publishedVersionSummaryReason :: Maybe Text
  , publishedVersionSummaryLive :: Bool
  -- ^ Whether this is the version the marketplace is handing out.
  , publishedVersionSummaryCreatedAt :: UTCTime
  }
  deriving stock Generic

instance ToJSON PublishedVersionSummary where
  toJSON = genericToJSON $ aesonOptions $ Just "publishedVersionSummary"
  toEncoding = genericToEncoding $ aesonOptions $ Just "publishedVersionSummary"

instance ToJSON PublishedCardSetResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "publishedCardSetResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "publishedCardSetResponse"

{- | One set, in full: its listing and every card in the version being looked at.

The listing comes along so a page showing one set needs one request rather than
this plus a sweep of the whole marketplace to find the row it belongs to.
-}
data PublishedCardSetVersionResponse = PublishedCardSetVersionResponse
  { publishedCardSetVersionResponseListing :: PublishedCardSetResponse
  , publishedCardSetVersionResponseVersion :: Int
  , publishedCardSetVersionResponseName :: Text
  , publishedCardSetVersionResponseNote :: Maybe Text
  , publishedCardSetVersionResponseCards :: [CustomCard]
  , publishedCardSetVersionResponseCreatedAt :: UTCTime
  }
  deriving stock Generic

instance ToJSON PublishedCardSetVersionResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "publishedCardSetVersionResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "publishedCardSetVersionResponse"

data PublishPost = PublishPost
  { publishNote :: Maybe Text
  , publishNotify :: Maybe Bool
  {- ^ Whether to email the author the review decision. Absent means yes: an
  older client that does not know to ask still gets told what happened.
  -}
  }
  deriving stock Generic

instance FromJSON PublishPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "publish"

{- | What the author is changing about a listing without republishing it.

Both fields are three-valued, as they are on the set: absent leaves what is
there, null clears it. So a client that knows about descriptions but not links
cannot blank a link by saving a blurb.
-}
data ListingPut = ListingPut
  { listingDescription :: Maybe (Maybe Text)
  , listingUrl :: Maybe (Maybe Text)
  }

instance FromJSON ListingPut where
  parseJSON = withObject "ListingPut" \o ->
    ListingPut <$> o .:! "description" <*> o .:! "url"

{- | A description as it is stored: blank is nothing, and trimmed, so a field
someone typed a space into does not read as a description wherever one is shown.
-}
normalizeDescription :: Maybe Text -> Maybe Text
normalizeDescription raw = do
  text <- T.strip <$> raw
  guard $ not (T.null text)
  pure text

-- | Which version to take. Absent means the newest.
newtype SubscribePost = SubscribePost {subscribeVersion :: Maybe Int}
  deriving stock Generic

instance FromJSON SubscribePost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "subscribe"

{- | Which of the two ways a set is being taken.

'Following' leaves the copy locked and on a recorded version, so it stays what
was published. 'Owned' hands over an ordinary set of the caller's.
-}
data Taking = Following | Owned

-- | The cards of a version, or a 500 that says which version could not be read.
versionCards :: ArkhamPublishedCardSetVersion -> Handler [CustomCard]
versionCards row =
  case parseEither parseJSON (arkhamPublishedCardSetVersionCards row) of
    Right cards -> pure cards
    Left err ->
      sendResponseStatus Status.status500
        $ object ["error" .= ("Could not read the published cards: " <> T.pack err)]

-- | Say you liked a set. Asking twice is the same as asking once.
postApiV1ArkhamPublishedCardSetLikeR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamPublishedCardSetLikeR publishedId = do
  userId <- getRequestUserId
  row <- runDB $ get404 publishedId
  now <- liftIO getCurrentTime
  existing <- runDB $ P.getBy (UniquePublishedCardSetLike publishedId userId)
  when (isNothing existing)
    $ runDB
    $ P.insert_
    $ ArkhamPublishedCardSetLike publishedId userId now
  listingFor userId (Entity publishedId row)

-- | Take it back. Not having liked it is already the outcome, so this cannot fail.
deleteApiV1ArkhamPublishedCardSetLikeR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
deleteApiV1ArkhamPublishedCardSetLikeR publishedId = do
  userId <- getRequestUserId
  row <- runDB $ get404 publishedId
  runDB
    $ P.deleteWhere
      [ ArkhamPublishedCardSetLikePublishedCardSetId P.==. publishedId
      , ArkhamPublishedCardSetLikeUserId P.==. userId
      ]
  listingFor userId (Entity publishedId row)

{- | How many cards a listing carries. Enough to fill a row at any width the
listing is shown at; the extra are clipped by the layout rather than sent and
thrown away.
-}
previewSize :: Int
previewSize = 12

{- | The marketplace, newest first: everything approved, plus your own listings
whatever state they are in.

Yours are included because this page is also where you find out what happened to
what you submitted. Nobody else's unapproved set appears, which is the point of
reviewing them.

@?mine=true@ narrows it to your own, which is a page of its own: an author's
listings outlive the sets they were published from, so a listing whose set has
since been deleted is only reachable from here.
-}
getApiV1ArkhamPublishedCardSetsR :: Handler [PublishedCardSetResponse]
getApiV1ArkhamPublishedCardSetsR = do
  userId <- getRequestUserId
  mine <- (== Just "true") <$> lookupGetParam "mine"
  let whose
        | mine = [ArkhamPublishedCardSetUserId P.==. userId]
        | otherwise =
            [ArkhamPublishedCardSetApprovedVersion P.!=. Nothing]
              P.||. [ArkhamPublishedCardSetUserId P.==. userId]
  rows <- runDB $ P.selectList whose [P.Desc ArkhamPublishedCardSetUpdatedAt]
  traverse (listingFor userId) rows

{- | The newest submission against a listing, which is what its review state is.

Ordered by version rather than by when it was written: a version is submitted
once, and the highest-numbered one is the latest thing said about the set.
-}
newestSubmission
  :: ArkhamPublishedCardSetId -> Handler (Maybe ArkhamCardSetSubmission)
newestSubmission publishedId =
  fmap (fmap entityVal)
    $ runDB
    $ P.selectFirst
      [ArkhamCardSetSubmissionPublishedCardSetId P.==. publishedId]
      [P.Desc ArkhamCardSetSubmissionVersion]

{- | The version a listing describes: the newest approved one, or -- for a set of
your own that has never passed -- the newest one at all, so you can see what you
submitted while it waits.
-}
describedVersion
  :: Bool
  -> ArkhamPublishedCardSetId
  -> ArkhamPublishedCardSet
  -> Handler (Maybe ArkhamPublishedCardSetVersion)
describedVersion mine publishedId row = case arkhamPublishedCardSetApprovedVersion row of
  Just version ->
    fmap entityVal <$> runDB (P.getBy (UniquePublishedCardSetVersion publishedId version))
  Nothing
    | mine ->
        fmap entityVal
          <$> runDB
            ( P.selectFirst
                [ArkhamPublishedCardSetVersionPublishedCardSetId P.==. publishedId]
                [P.Desc ArkhamPublishedCardSetVersionVersion]
            )
    | otherwise -> pure Nothing

listingFor :: UserId -> Entity ArkhamPublishedCardSet -> Handler PublishedCardSetResponse
listingFor userId (Entity publishedId row) = do
  let isAuthor = arkhamPublishedCardSetUserId row == userId
  latest <- describedVersion isAuthor publishedId row
  submission <- newestSubmission publishedId
  author <- runDB $ DB.get (arkhamPublishedCardSetUserId row)
  {- One of *your* sets following this listing, whichever set that is. Every
  subscription has to be looked at rather than the first one: on a set other
  people have taken, the first row found is somebody else's, and stopping there
  would report you as not subscribed and take your update away. -}
  subscriptions <-
    runDB
      $ P.selectList
        [ ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId
        , ArkhamCardSetSubscriptionSubscribed P.==. True
        ]
        []
  subscribed <- fmap (listToMaybe . catMaybes) . for subscriptions $ \(Entity _ sub) -> do
    owner <- runDB $ DB.get (arkhamCardSetSubscriptionCustomCardSetId sub)
    pure $ case owner of
      Just s | arkhamCustomCardSetUserId s == userId -> Just (arkhamCardSetSubscriptionVersion sub)
      _ -> Nothing
  cards <- maybe (pure []) versionCards latest
  likes <- runDB $ P.count [ArkhamPublishedCardSetLikePublishedCardSetId P.==. publishedId]
  liked <-
    runDB
      $ isJust
      <$> P.getBy (UniquePublishedCardSetLike publishedId userId)
  versions <- if isAuthor then versionHistory publishedId row else pure []
  pure
    $ PublishedCardSetResponse
      { publishedCardSetResponseId = publishedId
      , publishedCardSetResponseName = arkhamPublishedCardSetName row
      , publishedCardSetResponseDescription = arkhamPublishedCardSetDescription row
      , publishedCardSetResponseUrl = arkhamPublishedCardSetUrl row
      , publishedCardSetResponseIcon = arkhamPublishedCardSetIcon row
      , publishedCardSetResponseInvestigatorExpansion =
          arkhamPublishedCardSetInvestigatorExpansion row
      , publishedCardSetResponseAuthor = maybe "someone" userUsername author
      , publishedCardSetResponseMine = isAuthor
      , publishedCardSetResponseOfficial = maybe False userAdmin author
      , publishedCardSetResponseLatestVersion =
          fromMaybe 0 (arkhamPublishedCardSetApprovedVersion row)
      , publishedCardSetResponseReviewStatus =
          maybe "none" arkhamCardSetSubmissionStatus submission
      , publishedCardSetResponsePendingVersion = do
          sub <- submission
          guard isAuthor
          guard $ arkhamCardSetSubmissionStatus sub == submissionPending
          pure $ arkhamCardSetSubmissionVersion sub
      , publishedCardSetResponseDenialReason = do
          sub <- submission
          guard isAuthor
          guard $ arkhamCardSetSubmissionStatus sub == submissionDenied
          arkhamCardSetSubmissionReason sub
      , publishedCardSetResponseCardCount = length cards
      , publishedCardSetResponseNote = arkhamPublishedCardSetVersionNote =<< latest
      , publishedCardSetResponsePreview = take previewSize cards
      , publishedCardSetResponseLikes = likes
      , publishedCardSetResponseLiked = liked
      , publishedCardSetResponseUpdatedAt = arkhamPublishedCardSetUpdatedAt row
      , publishedCardSetResponseSubscribedVersion = subscribed
      , publishedCardSetResponseVersions = versions
      }

{- | Every version of a listing, newest first, each with what was decided about
it.

One query for the versions and one for the submissions, paired up here rather
than joined per row: a listing has a handful of versions, and this is only ever
built for the author's own.
-}
versionHistory
  :: ArkhamPublishedCardSetId -> ArkhamPublishedCardSet -> Handler [PublishedVersionSummary]
versionHistory publishedId row = do
  versions <-
    runDB
      $ P.selectList
        [ArkhamPublishedCardSetVersionPublishedCardSetId P.==. publishedId]
        [P.Desc ArkhamPublishedCardSetVersionVersion]
  submissions <-
    runDB
      $ P.selectList [ArkhamCardSetSubmissionPublishedCardSetId P.==. publishedId] []
  let decided =
        [(arkhamCardSetSubmissionVersionId sub, sub) | Entity _ sub <- submissions]
  pure
    [ PublishedVersionSummary
        { publishedVersionSummaryVersion = arkhamPublishedCardSetVersionVersion version
        , publishedVersionSummaryNote = arkhamPublishedCardSetVersionNote version
        , publishedVersionSummaryStatus = maybe "none" arkhamCardSetSubmissionStatus submission
        , publishedVersionSummaryReason = arkhamCardSetSubmissionReason =<< submission
        , publishedVersionSummaryLive =
            arkhamPublishedCardSetApprovedVersion row
              == Just (arkhamPublishedCardSetVersionVersion version)
        , publishedVersionSummaryCreatedAt = arkhamPublishedCardSetVersionCreatedAt version
        }
    | Entity versionId version <- versions
    , let submission = snd <$> find ((== versionId) . fst) decided
    ]

{- | A version to look at or import. `?version=` for an older one.

Only approved versions, unless you are the author looking at your own -- which
is how you see what you have waiting, and what you got back.
-}
getApiV1ArkhamPublishedCardSetR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetVersionResponse
getApiV1ArkhamPublishedCardSetR publishedId = do
  userId <- getRequestUserId
  published <- runDB $ get404 publishedId
  let mine = arkhamPublishedCardSetUserId published == userId
  listing <- listingFor userId (Entity publishedId published)
  wanted <- lookupGetParam "version"
  row <-
    if mine
      then anyVersion publishedId (readMaybe . T.unpack =<< wanted)
      else requireVersion publishedId (readMaybe . T.unpack =<< wanted)
  cards <- versionCards row
  pure
    $ PublishedCardSetVersionResponse
      { publishedCardSetVersionResponseListing = listing
      , publishedCardSetVersionResponseVersion = arkhamPublishedCardSetVersionVersion row
      , publishedCardSetVersionResponseName = arkhamPublishedCardSetVersionName row
      , publishedCardSetVersionResponseNote = arkhamPublishedCardSetVersionNote row
      , publishedCardSetVersionResponseCards = cards
      , publishedCardSetVersionResponseCreatedAt = arkhamPublishedCardSetVersionCreatedAt row
      }

{- | An approved version: the one asked for, or the newest that has passed.

A version that was denied, or is still waiting, is not here -- it is not in the
marketplace, and notFound is what being absent from the marketplace means. The
specific version is checked against its own submission rather than against the
listing's approved high-water mark, because approving v3 says nothing about a v2
that was turned down.
-}
requireVersion
  :: ArkhamPublishedCardSetId -> Maybe Int -> Handler ArkhamPublishedCardSetVersion
requireVersion publishedId wanted = do
  published <- runDB $ get404 publishedId
  version <- case wanted of
    Just version -> pure version
    Nothing -> maybe notFound pure (arkhamPublishedCardSetApprovedVersion published)
  found <- runDB $ P.getBy (UniquePublishedCardSetVersion publishedId version)
  case found of
    Nothing -> notFound
    Just (Entity versionId row) -> do
      approved <- versionApproved versionId
      if approved then pure row else notFound

-- | Whether this exact version is one a reviewer has let through.
versionApproved :: ArkhamPublishedCardSetVersionId -> Handler Bool
versionApproved versionId = do
  submission <- runDB $ P.getBy (UniqueCardSetSubmissionVersion versionId)
  pure $ case submission of
    Just (Entity _ sub) -> arkhamCardSetSubmissionStatus sub == submissionApproved
    Nothing -> False

{- | The author's own view, where review state does not hide anything. Defaults to
the newest version rather than the newest approved one, because what they came to
look at is what they last submitted.
-}
anyVersion :: ArkhamPublishedCardSetId -> Maybe Int -> Handler ArkhamPublishedCardSetVersion
anyVersion publishedId wanted = do
  found <- case wanted of
    Just version -> runDB $ P.getBy (UniquePublishedCardSetVersion publishedId version)
    Nothing ->
      runDB
        $ P.selectFirst
          [ArkhamPublishedCardSetVersionPublishedCardSetId P.==. publishedId]
          [P.Desc ArkhamPublishedCardSetVersionVersion]
  maybe notFound (pure . entityVal) found

{- | Rewrite what a listing says about itself, without republishing it.

Only the description and the link: the name and the cards are the version's, and
changing those is publishing a new one. Nothing is re-reviewed, because nothing
a reviewer looked at has changed -- a listing's blurb is not its contents.

Both are written back onto the author's own copy of the set when the listing
still points at one, so the two do not drift and the next publish does not
quietly undo the edit made here.
-}
putApiV1ArkhamPublishedCardSetR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
putApiV1ArkhamPublishedCardSetR publishedId = do
  userId <- getRequestUserId
  row <- runDB $ get404 publishedId
  unless (arkhamPublishedCardSetUserId row == userId) $ permissionDenied "Not your published set"
  ListingPut {listingDescription, listingUrl} <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  let description = case listingDescription of
        Nothing -> arkhamPublishedCardSetDescription row
        Just given -> normalizeDescription given
  url <- case listingUrl of
    Nothing -> pure $ arkhamPublishedCardSetUrl row
    Just given -> normalizeUrl given
  runDB do
    P.update
      publishedId
      [ ArkhamPublishedCardSetDescription P.=. description
      , ArkhamPublishedCardSetUrl P.=. url
      , ArkhamPublishedCardSetUpdatedAt P.=. now
      ]
    for_ (arkhamPublishedCardSetCustomCardSetId row) \setId ->
      P.update
        setId
        [ ArkhamCustomCardSetDescription P.=. description
        , ArkhamCustomCardSetUrl P.=. url
        ]
  listingFor userId
    $ Entity publishedId
    $ row
      { arkhamPublishedCardSetDescription = description
      , arkhamPublishedCardSetUrl = url
      , arkhamPublishedCardSetUpdatedAt = now
      }

{- | Unlist a set. The versions go with it, along with anything of it still in the
review queue, and anyone following it is unsubscribed by the foreign key rather
than left pointing at nothing.
-}
deleteApiV1ArkhamPublishedCardSetR :: ArkhamPublishedCardSetId -> Handler ()
deleteApiV1ArkhamPublishedCardSetR publishedId = do
  userId <- getRequestUserId
  row <- runDB $ get404 publishedId
  unless (arkhamPublishedCardSetUserId row == userId) $ permissionDenied "Not your published set"
  runDB do
    P.deleteWhere [ArkhamCardSetSubmissionPublishedCardSetId P.==. publishedId]
    P.deleteWhere [ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId]
    P.deleteWhere [ArkhamPublishedCardSetVersionPublishedCardSetId P.==. publishedId]
    P.delete publishedId

{- | Submit the set as a new version, or -- for an admin -- list it.

Nothing about what the set /is/ is asked for here: the name, the blurb and the
link are the set's own and are taken as they stand.

The cards are snapshotted as they stand, so the version stays importable however
the author's own copy changes afterwards -- and so that what a reviewer looked at
is what anyone importing it gets.

For anybody else nothing is listed by this. The version goes into the review
queue, and the marketplace keeps showing whichever version was last approved until
someone acts on this one.

Submitting again while something is still waiting replaces it rather than queueing
a second thing: the author has changed their mind about what they are asking for,
and there is no sense reviewing a version they have already moved past. The
superseded snapshot goes with it, since nothing could ever have imported it.
-}
postApiV1ArkhamCustomCardSetPublishR
  :: ArkhamCustomCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamCustomCardSetPublishR setId = do
  Entity userId user <- getRequestUser
  setRow <- ownedCardSet userId setId
  -- Somebody else's set, taken from a listing or a link. Publishing it would be
  -- putting their work up as yours.
  requireUnlocked setId
  PublishPost {publishNote, publishNotify} <- requireCheckJsonBody
  now <- liftIO getCurrentTime

  cards <- runDB $ P.selectList [ArkhamCustomCardCustomCardSetId P.==. setId] []
  when (null cards) $ invalidArgs ["There is nothing in that set to publish"]

  {- The blurb and the link are the set's, copied onto the listing as it stands
  rather than asked for here: they say what the set is, which is not a thing
  about this one version, and a set says them whether or not it is listed. The
  listing keeps its own copy so it still reads if the author's set is deleted,
  and editing either side writes to both. -}
  let description = arkhamCustomCardSetDescription setRow
      url = arkhamCustomCardSetUrl setRow
      name = arkhamCustomCardSetName setRow
      -- The icon and whether this is an expansion travel with them, so a
      -- listing can be drawn without loading the set behind it. What the set
      -- \*declares* -- its tokens and keywords -- is not copied here: it is
      -- stamped on every card in the snapshot, which is where an import of a
      -- version reads it from.
      icon = arkhamCustomCardSetIcon setRow
      expansion = arkhamCustomCardSetInvestigatorExpansion setRow
      -- Printed order, which is the order the author put them in. It is fixed
      -- here rather than by whoever reads the version, because the listing only
      -- carries the first few and they have to be the first few.
      ordered = sortOn (printedOrder . arkhamCustomCardDef . entityVal) cards
      snapshot =
        toJSON
          [ object ["def" .= arkhamCustomCardDef card, "art" .= arkhamCustomCardArt card]
          | Entity _ card <- ordered
          ]

  existing <-
    runDB
      $ P.selectFirst
        [ ArkhamPublishedCardSetUserId P.==. userId
        , ArkhamPublishedCardSetCustomCardSetId P.==. Just setId
        ]
        []

  let publication = if user.admin then Listed userId else ForReview
      -- Nothing to be told about a decision you have just made yourself, so an
      -- admin's row does not claim mail is owed.
      notify = case publication of
        Listed _ -> False
        ForReview -> fromMaybe True publishNotify

  publishedId <- runDB case existing of
    Just (Entity found row) -> do
      withdrawPending found
      let version = arkhamPublishedCardSetLatestVersion row + 1
      P.update found
        $ [ ArkhamPublishedCardSetName P.=. name
          , ArkhamPublishedCardSetDescription P.=. description
          , ArkhamPublishedCardSetUrl P.=. url
          , ArkhamPublishedCardSetIcon P.=. icon
          , ArkhamPublishedCardSetInvestigatorExpansion P.=. expansion
          , ArkhamPublishedCardSetLatestVersion P.=. version
          , ArkhamPublishedCardSetUpdatedAt P.=. now
          ]
        <> listedUpdates publication version (arkhamPublishedCardSetApprovedVersion row)
      versionId <-
        P.insert $ ArkhamPublishedCardSetVersion found version publishNote name snapshot now
      submit publication found versionId userId version notify now
      pure found
    Nothing -> do
      let approved = case publication of
            Listed _ -> Just 1
            ForReview -> Nothing
      found <-
        P.insert
          $ ArkhamPublishedCardSet
            userId
            (Just setId)
            name
            description
            url
            icon
            expansion
            1
            approved
            now
            now
      versionId <-
        P.insert $ ArkhamPublishedCardSetVersion found 1 publishNote name snapshot now
      submit publication found versionId userId 1 notify now
      pure found

  row <- runDB $ get404 publishedId
  listingFor userId (Entity publishedId row)

{- | What publishing does with the version it has just made.

'Listed' carries who listed it, because an approval records the person who made
it and here that is the author.
-}
data Publication = ForReview | Listed UserId

{- | Raise the listing's approved version, for a publish that is already approved.

Only ever upwards, the same rule reviewing follows: a publish cannot put the
marketplace back to an older snapshot than the one people are subscribed to. In
practice the new version is always the highest, so this is a guard rather than a
decision.
-}
listedUpdates :: Publication -> Int -> Maybe Int -> [P.Update ArkhamPublishedCardSet]
listedUpdates ForReview _ _ = []
listedUpdates (Listed _) version approved =
  [ArkhamPublishedCardSetApprovedVersion P.=. Just (max version (fromMaybe version approved))]

{- | Record the version's submission: queued, or decided on the spot.

An admin's is written as approved by them rather than skipped entirely, so the
review queue's history is still the whole history of what reached the
marketplace, and 'versionApproved' needs no second rule to let the version be
imported.
-}
submit
  :: Publication
  -> ArkhamPublishedCardSetId
  -> ArkhamPublishedCardSetVersionId
  -> UserId
  -> Int
  -> Bool
  -> UTCTime
  -> SqlPersistT Handler ()
submit publication publishedId versionId userId version notify now =
  P.insert_
    $ ArkhamCardSetSubmission
      publishedId
      versionId
      userId
      version
      status
      notify
      Nothing
      reviewedBy
      reviewedAt
      now
      now
 where
  (status, reviewedBy, reviewedAt) = case publication of
    ForReview -> (submissionPending, Nothing, Nothing)
    Listed reviewerId -> (submissionApproved, Just reviewerId, Just now)

{- | Drop whatever this listing had waiting, snapshot and all.

Only ever pending rows: a decision that has been made is a record, and an
approved version is something people may be subscribed to.
-}
withdrawPending :: ArkhamPublishedCardSetId -> SqlPersistT Handler ()
withdrawPending publishedId = do
  pending <-
    P.selectList
      [ ArkhamCardSetSubmissionPublishedCardSetId P.==. publishedId
      , ArkhamCardSetSubmissionStatus P.==. submissionPending
      ]
      []
  P.deleteWhere [ArkhamCardSetSubmissionId P.<-. map entityKey pending]
  P.deleteWhere
    [ArkhamPublishedCardSetVersionId P.<-. map (arkhamCardSetSubmissionVersionId . entityVal) pending]

{- | Take a published set into your own collection, following it from then on.

The copy is locked: it is what was published, and taking it again is how it
catches up. 'postApiV1ArkhamPublishedCardSetImportR' is the other way to take it,
for somebody who wants the cards to build on rather than to follow.

Only an approved version: 'requireVersion' answers notFound for anything else, so
a set waiting for review cannot be imported by url even by its own author.

The import is the file import's: prepare every card (which is where art is
uploaded and can fail) before emptying and refilling the set, so a set that is
already here is never left as half of what was replacing it.
-}
postApiV1ArkhamPublishedCardSetSubscribeR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamPublishedCardSetSubscribeR publishedId = do
  userId <- getRequestUserId
  published <- runDB $ get404 publishedId
  SubscribePost {subscribeVersion} <- requireCheckJsonBody
  version <- requireVersion publishedId subscribeVersion
  void $ importVersion Following userId publishedId version
  listingFor userId (Entity publishedId published)

{- | Take a published set as your own: the cards, and nothing following them.

The same write as subscribing, and the same record of which set of yours is your
copy of this listing -- only the flag saying you follow it is off, so the copy is
yours to edit and hears nothing more from here. Taking it the other way replaces
it, since a card code is unique per account and nobody can hold both.
-}
postApiV1ArkhamPublishedCardSetImportR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamPublishedCardSetImportR publishedId = do
  userId <- getRequestUserId
  published <- runDB $ get404 publishedId
  SubscribePost {subscribeVersion} <- requireCheckJsonBody
  version <- requireVersion publishedId subscribeVersion
  void $ importVersion Owned userId publishedId version
  listingFor userId (Entity publishedId published)

{- | Bring a subscribed set up to the newest approved version.

Only a set that is still following one can be updated; an edited copy has to be
taken again from the marketplace instead, which is what its lost subscription
means. A version the author has submitted but nobody has approved is not an
update -- 'requireVersion' will not hand it over, and the set stays where it is.
-}
postApiV1ArkhamCustomCardSetSyncR
  :: ArkhamCustomCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamCustomCardSetSyncR setId = do
  userId <- getRequestUserId
  void $ ownedCardSet userId setId
  subscription <-
    runDB
      $ P.selectFirst
        [ ArkhamCardSetSubscriptionCustomCardSetId P.==. setId
        , ArkhamCardSetSubscriptionSubscribed P.==. True
        ]
        []
  case subscription of
    Nothing -> invalidArgs ["That set is not following a published one any more"]
    Just (Entity _ sub) -> do
      let publishedId = arkhamCardSetSubscriptionPublishedCardSetId sub
      published <- runDB $ get404 publishedId
      version <- requireVersion publishedId Nothing
      void $ importVersion Following userId publishedId version
      listingFor userId (Entity publishedId published)

{- | Write a published version into the caller's collection and record which set
of theirs is their copy of this listing. The set is matched by the row that
already points at it, so taking it again replaces the copy in place rather than
making a second one -- which holds whichever way either take was done.

'Following' leaves the copy locked and on this version. 'Owned' writes the same
row with the flag off: the caller has a set of their own, and the row is there
only so the next take finds it.
-}
importVersion
  :: Taking
  -> UserId
  -> ArkhamPublishedCardSetId
  -> ArkhamPublishedCardSetVersion
  -> Handler ArkhamCustomCardSetId
importVersion taking userId publishedId version = do
  cards <- versionCards version
  now <- liftIO getCurrentTime
  let name = arkhamPublishedCardSetVersionName version
  -- The listing's blurb and link come with the set, so a copy of it says what
  -- it is and where it came from rather than arriving as a name and a pile of
  -- cards.
  listing <- runDB $ DB.get publishedId
  let description = arkhamPublishedCardSetDescription =<< listing
      url = arkhamPublishedCardSetUrl =<< listing
      icon = arkhamPublishedCardSetIcon =<< listing
      expansion = maybe False arkhamPublishedCardSetInvestigatorExpansion listing
      {- What the set declared, read back off the cards the version was
      published with. The version carries no room of its own for it, and does
      not need one: every card in it was stamped from the author's row, so the
      cards are the record. -}
      (tokens, keywords) = setDeclarationsFromCards cards

  {- Not guarded by 'requireUnlocked': the copy this replaces is a copy of this
  very set, so it is either unlocked or locked by this set -- and refusing the
  second would be refusing to let a followed set catch up, which is the whole
  point of following one. -}
  existing <- myCopyOf userId publishedId
  {- A name none of your other sets is using. The copy being replaced is excepted
  from that, since the name it already holds is its own -- but a third set of
  yours holding it is still a clash, and renaming into one is a unique index
  violation rather than an update. -}
  name' <- freeName userId existing name

  let incoming =
        ArkhamCustomCardSet
          userId
          name'
          description
          url
          Nothing
          expansion
          icon
          tokens
          keywords
          now
          now
  prepared <- traverse (prepareCardForSet userId incoming) cards

  runDB do
    setId <- case existing of
      Just found -> do
        P.update
          found
          [ ArkhamCustomCardSetName P.=. name'
          , ArkhamCustomCardSetDescription P.=. description
          , ArkhamCustomCardSetUrl P.=. url
          , ArkhamCustomCardSetInvestigatorExpansion P.=. expansion
          , ArkhamCustomCardSetIcon P.=. icon
          , ArkhamCustomCardSetTokens P.=. tokens
          , ArkhamCustomCardSetKeywords P.=. keywords
          , ArkhamCustomCardSetUpdatedAt P.=. now
          ]
        P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. found]
        pure found
      Nothing -> P.insert incoming
    for_ prepared $ persistCard userId setId now
    -- Written after the cards, so a failed import leaves nothing claiming to be
    -- up to date.
    followOnly FromListing setId
    P.deleteWhere [ArkhamCardSetSubscriptionCustomCardSetId P.==. setId]
    P.insert_
      $ ArkhamCardSetSubscription
        setId
        publishedId
        (case taking of Following -> True; Owned -> False)
        (arkhamPublishedCardSetVersionVersion version)
        now
        now
    pure setId

{- | The caller's own copy of this listing, if they have one.

Whether or not it still follows it: the row is what says "this set of mine came
from that listing", and a copy taken into the editor is still the set that a
later subscribe has to replace.

A copy taken from the author's /link/ before they published counts too, which is
what 'myCopyOfSet' is asked for first. Without that, subscribing to a set you
already hold by link writes a second set and moves the cards into it, leaving the
first one empty -- a card code is unique per account, so the two cannot coexist.

The subscription scan below is the fallback for a listing whose author has since
deleted their own set, which leaves nothing for the first question to ask about.
-}
myCopyOf :: UserId -> ArkhamPublishedCardSetId -> Handler (Maybe ArkhamCustomCardSetId)
myCopyOf userId publishedId = do
  source <- runDB $ fmap (arkhamPublishedCardSetCustomCardSetId =<<) (DB.get publishedId)
  byAuthorSet <- maybe (pure Nothing) (myCopyOfSet userId) source
  maybe (myCopyOfListing userId publishedId) (pure . Just) byAuthorSet

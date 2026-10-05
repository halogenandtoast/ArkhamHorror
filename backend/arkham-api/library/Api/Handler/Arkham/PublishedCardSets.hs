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

Subscribing runs the same import a file import does, and records which version it
landed on, so the copy can be brought up to date later. Editing that copy deletes
the record -- what is in it is then not what was published -- which is the whole
of the "subscribed or not" rule.
-}
module Api.Handler.Arkham.PublishedCardSets (
  getApiV1ArkhamPublishedCardSetsR,
  getApiV1ArkhamPublishedCardSetR,
  putApiV1ArkhamPublishedCardSetR,
  deleteApiV1ArkhamPublishedCardSetR,
  postApiV1ArkhamPublishedCardSetSubscribeR,
  postApiV1ArkhamCustomCardSetPublishR,
  postApiV1ArkhamCustomCardSetSyncR,
  postApiV1ArkhamPublishedCardSetLikeR,
  deleteApiV1ArkhamPublishedCardSetLikeR,
  listingFor,
  versionCards,
) where

import Api.Handler.Arkham.CustomCards (normalizeUrl, ownedCardSet, persistCard, prepareCardForSet)
import Arkham.Card.CustomCard (CustomCard (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Time.Clock
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

{- | A card's place in its set: the number printed on it, read out of its own def.

Sorted on the leading digits rather than the text, so 10 follows 9. A card with no
number sorts last, since it has said nothing about where it goes.
-}
printedOrder :: Value -> (Int, Text)
printedOrder value = (maybe maxBound id (readMaybe (T.unpack digits)), number)
 where
  number = case value of
    Object o -> case KeyMap.lookup "meta" o of
      Just (Object m) -> case KeyMap.lookup "number" m of
        Just (String t) -> t
        Just (Number n) -> tshow (round n :: Int)
        _ -> ""
      _ -> ""
    _ -> ""
  digits = T.takeWhile isDigit number

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
    runDB $ P.selectList [ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId] []
  subscribed <- fmap (listToMaybe . catMaybes) $ forM subscriptions \(Entity _ sub) -> do
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
          $ ArkhamPublishedCardSet userId (Just setId) name description url 1 approved now now
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
  void $ importVersion userId publishedId version
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
  subscription <- runDB $ P.getBy (UniqueCardSetSubscription setId)
  case subscription of
    Nothing -> invalidArgs ["That set is not following a published one any more"]
    Just (Entity _ sub) -> do
      let publishedId = arkhamCardSetSubscriptionPublishedCardSetId sub
      published <- runDB $ get404 publishedId
      version <- requireVersion publishedId Nothing
      void $ importVersion userId publishedId version
      listingFor userId (Entity publishedId published)

{- | Write a published version into the caller's collection and record that it is
following it. The set is matched by the subscription that already points at it,
so updating replaces the copy in place rather than making a second one.
-}
importVersion
  :: UserId
  -> ArkhamPublishedCardSetId
  -> ArkhamPublishedCardSetVersion
  -> Handler ArkhamCustomCardSetId
importVersion userId publishedId version = do
  cards <- versionCards version
  now <- liftIO getCurrentTime
  let name = arkhamPublishedCardSetVersionName version
  -- The listing's blurb and link come with the set, so a copy of it says what
  -- it is and where it came from rather than arriving as a name and a pile of
  -- cards.
  listing <- runDB $ DB.get publishedId
  let description = arkhamPublishedCardSetDescription =<< listing
      url = arkhamPublishedCardSetUrl =<< listing

  existing <- mineFollowing userId publishedId
  -- A first subscription has to find a name nobody else of yours is using.
  name' <- case existing of
    Just _ -> pure name
    Nothing -> freeName userId name

  prepared <- traverse (prepareCardForSet userId name') cards

  runDB do
    setId <- case existing of
      Just found -> do
        P.update
          found
          [ ArkhamCustomCardSetName P.=. name'
          , ArkhamCustomCardSetDescription P.=. description
          , ArkhamCustomCardSetUrl P.=. url
          , ArkhamCustomCardSetUpdatedAt P.=. now
          ]
        P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. found]
        pure found
      Nothing -> P.insert $ ArkhamCustomCardSet userId name' description url Nothing now now
    for_ prepared $ persistCard userId setId now
    -- Written after the cards, so a failed import leaves nothing claiming to be
    -- up to date.
    P.deleteWhere [ArkhamCardSetSubscriptionCustomCardSetId P.==. setId]
    P.insert_
      $ ArkhamCardSetSubscription
        setId
        publishedId
        (arkhamPublishedCardSetVersionVersion version)
        now
        now
    pure setId

-- | The caller's own set following this listing, if they have one.
mineFollowing :: UserId -> ArkhamPublishedCardSetId -> Handler (Maybe ArkhamCustomCardSetId)
mineFollowing userId publishedId = do
  subs <-
    runDB $ P.selectList [ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId] []
  ours <- forM subs \(Entity _ sub) -> do
    let setId = arkhamCardSetSubscriptionCustomCardSetId sub
    owner <- runDB $ DB.get setId
    pure $ case owner of
      Just s | arkhamCustomCardSetUserId s == userId -> Just setId
      _ -> Nothing
  pure $ listToMaybe (catMaybes ours)

{- | A name no other set of yours holds. Subscribing to something called what one
of your own sets is called should not fail, and should not silently replace it.
-}
freeName :: UserId -> Text -> Handler Text
freeName userId name = go name (2 :: Int)
 where
  go candidate attempt = do
    taken <- runDB $ P.getBy (UniqueUserCustomCardSetName userId candidate)
    case taken of
      Nothing -> pure candidate
      Just _
        | attempt > 50 -> invalidArgs ["You already have a set called \"" <> name <> "\""]
        | otherwise -> go (name <> " (" <> tshow attempt <> ")") (attempt + 1)

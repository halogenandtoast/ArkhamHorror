{- | Reviewing what people have submitted to the card marketplace.

'Api.Handler.Arkham.PublishedCardSets' is the other half: publishing a set
snapshots it into a version and queues that version here, and nothing is listed
or importable until one of these handlers approves it.

A decision is recorded on the submission and, when it is an approval, raised onto
the listing as @approvedVersion@ -- the one number every marketplace read needs,
which is why it is kept there rather than worked out from this table each time.

The author is emailed if they asked to be when they submitted. The mail is sent
after the decision is written and cannot fail it: a review that went through has
gone through whether or not their mail host was reachable.

All of these are under @\/api\/v1\/admin@, which 'Foundation.isAuthorized' gates
on the admin flag, so none of them check it again.
-}
module Api.Handler.Arkham.CardSetSubmissions (
  getApiV1AdminCardSetSubmissionsR,
  getApiV1AdminCardSetSubmissionR,
  postApiV1AdminCardSetSubmissionApproveR,
  postApiV1AdminCardSetSubmissionDenyR,
) where

import Api.Handler.Arkham.PublishedCardSets (versionCards)
import Arkham.Card.CustomCard (CustomCard (..))
import Base.Mail (sendPlainTextEmail)
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist qualified as DB
import Import hiding ((==.))
import Import qualified as P
import Json hiding (Success)

{- | A submission as the review queue shows it: what was submitted, by whom, and
what has been decided.

The first few cards come along so the queue can be skimmed without opening every
row. The rest are behind the detail endpoint, since a set can be a hundred and
fifty cards and the queue is a list.
-}
data SubmissionResponse = SubmissionResponse
  { submissionResponseId :: ArkhamCardSetSubmissionId
  , submissionResponsePublishedCardSetId :: ArkhamPublishedCardSetId
  , submissionResponseSetName :: Text
  , submissionResponseSetDescription :: Maybe Text
  {- ^ What the author says the set is. The listing's, not the version's, so a
  reviewer reads the same blurb a browser would.
  -}
  , submissionResponseSetUrl :: Maybe Text
  {- ^ Where the author says the set lives, which is often the only way to
  check that a set is theirs to publish.
  -}
  , submissionResponseAuthor :: Text
  , submissionResponseAuthorEmail :: Text
  -- ^ So a reviewer can reach the author about something the form cannot say.
  , submissionResponseVersion :: Int
  , submissionResponseStatus :: Text
  , submissionResponseNote :: Maybe Text
  -- ^ What the author said about this version when they submitted it.
  , submissionResponseNotify :: Bool
  , submissionResponseReason :: Maybe Text
  , submissionResponseReviewedBy :: Maybe Text
  , submissionResponseReviewedAt :: Maybe UTCTime
  , submissionResponseSubmittedAt :: UTCTime
  , submissionResponseCardCount :: Int
  , submissionResponsePreview :: [CustomCard]
  , submissionResponseApprovedVersion :: Maybe Int
  {- ^ The version currently in the marketplace, so an update is visibly an
  update rather than looking like a first submission.
  -}
  }
  deriving stock Generic

instance ToJSON SubmissionResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "submissionResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "submissionResponse"

-- | One submission with every card in it, which is what reviewing it takes.
data SubmissionDetailResponse = SubmissionDetailResponse
  { submissionDetailResponseSubmission :: SubmissionResponse
  , submissionDetailResponseCards :: [CustomCard]
  }
  deriving stock Generic

instance ToJSON SubmissionDetailResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "submissionDetailResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "submissionDetailResponse"

newtype DenyPost = DenyPost {denyReason :: Text}
  deriving stock Generic

instance FromJSON DenyPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "deny"

-- | As many cards as a queue row can usefully show.
previewSize :: Int
previewSize = 12

{- | The review queue. @?status=@ narrows it; the default is what still needs
doing, since that is what the page is for.

Oldest first among pending, so the queue is a queue. Decided submissions come
back newest first, where what was just done is what is being looked for.
-}
getApiV1AdminCardSetSubmissionsR :: Handler [SubmissionResponse]
getApiV1AdminCardSetSubmissionsR = do
  wanted <- lookupGetParam "status"
  let (filters, order) = case wanted of
        Nothing ->
          ([ArkhamCardSetSubmissionStatus P.==. submissionPending], [P.Asc ArkhamCardSetSubmissionCreatedAt])
        Just "all" -> ([], [P.Desc ArkhamCardSetSubmissionCreatedAt])
        Just status
          | status == submissionPending ->
              ([ArkhamCardSetSubmissionStatus P.==. submissionPending], [P.Asc ArkhamCardSetSubmissionCreatedAt])
          | otherwise ->
              ([ArkhamCardSetSubmissionStatus P.==. status], [P.Desc ArkhamCardSetSubmissionCreatedAt])
  rows <- runDB $ P.selectList filters order
  traverse submissionResponse rows

-- | One submission, with all of its cards.
getApiV1AdminCardSetSubmissionR :: ArkhamCardSetSubmissionId -> Handler SubmissionDetailResponse
getApiV1AdminCardSetSubmissionR submissionId = do
  row <- runDB $ get404 submissionId
  response <- submissionResponse (Entity submissionId row)
  cards <- submissionCards row
  pure $ SubmissionDetailResponse response cards

-- | The cards of the version a submission is about.
submissionCards :: ArkhamCardSetSubmission -> Handler [CustomCard]
submissionCards row = do
  version <- runDB $ get404 (arkhamCardSetSubmissionVersionId row)
  versionCards version

submissionResponse :: Entity ArkhamCardSetSubmission -> Handler SubmissionResponse
submissionResponse (Entity submissionId row) = do
  published <- runDB $ DB.get (arkhamCardSetSubmissionPublishedCardSetId row)
  version <- runDB $ DB.get (arkhamCardSetSubmissionVersionId row)
  author <- runDB $ DB.get (arkhamCardSetSubmissionUserId row)
  reviewer <- runDB $ traverse DB.get (arkhamCardSetSubmissionReviewedByUserId row)
  cards <- maybe (pure []) versionCards version
  pure
    $ SubmissionResponse
      { submissionResponseId = submissionId
      , submissionResponsePublishedCardSetId = arkhamCardSetSubmissionPublishedCardSetId row
      , -- The version's own name, not the listing's: the listing carries whatever
        -- was published last, and a reviewer has to see what this one is called.
        submissionResponseSetName =
          maybe "" arkhamPublishedCardSetVersionName version
      , submissionResponseSetDescription = arkhamPublishedCardSetDescription =<< published
      , submissionResponseSetUrl = arkhamPublishedCardSetUrl =<< published
      , submissionResponseAuthor = maybe "someone" userUsername author
      , submissionResponseAuthorEmail = maybe "" userEmail author
      , submissionResponseVersion = arkhamCardSetSubmissionVersion row
      , submissionResponseStatus = arkhamCardSetSubmissionStatus row
      , submissionResponseNote = arkhamPublishedCardSetVersionNote =<< version
      , submissionResponseNotify = arkhamCardSetSubmissionNotify row
      , submissionResponseReason = arkhamCardSetSubmissionReason row
      , submissionResponseReviewedBy = userUsername <$> join reviewer
      , submissionResponseReviewedAt = arkhamCardSetSubmissionReviewedAt row
      , submissionResponseSubmittedAt = arkhamCardSetSubmissionCreatedAt row
      , submissionResponseCardCount = length cards
      , submissionResponsePreview = take previewSize cards
      , submissionResponseApprovedVersion =
          arkhamPublishedCardSetApprovedVersion =<< published
      }

{- | Approve it: the version goes into the marketplace.

@approvedVersion@ only ever rises. Approving an older version than the one already
out there is a reviewer working through a backlog, not a decision to put the
marketplace back -- and people are subscribed to the newer one.

@updatedAt@ is bumped because that is what orders the marketplace, and a set
arriving in it is news.
-}
postApiV1AdminCardSetSubmissionApproveR :: ArkhamCardSetSubmissionId -> Handler SubmissionResponse
postApiV1AdminCardSetSubmissionApproveR submissionId = do
  Entity reviewerId _ <- getAdminUser
  row <- runDB $ get404 submissionId
  requirePending row
  now <- liftIO getCurrentTime
  let publishedId = arkhamCardSetSubmissionPublishedCardSetId row
      version = arkhamCardSetSubmissionVersion row
  published <- runDB $ get404 publishedId

  runDB do
    P.update
      submissionId
      [ ArkhamCardSetSubmissionStatus P.=. submissionApproved
      , ArkhamCardSetSubmissionReason P.=. Nothing
      , ArkhamCardSetSubmissionReviewedByUserId P.=. Just reviewerId
      , ArkhamCardSetSubmissionReviewedAt P.=. Just now
      , ArkhamCardSetSubmissionUpdatedAt P.=. now
      ]
    P.update
      publishedId
      [ ArkhamPublishedCardSetApprovedVersion
          P.=. Just (max version (fromMaybe version (arkhamPublishedCardSetApprovedVersion published)))
      , ArkhamPublishedCardSetUpdatedAt P.=. now
      ]

  updated <- runDB $ get404 submissionId
  response <- submissionResponse (Entity submissionId updated)
  announce response Nothing
  pure response

{- | Turn it down, with the reason the author is going to read.

The version is kept rather than deleted: it is what was looked at, and the author
can be told about it in a sentence that names a version. It stays unimportable,
because only an approved submission makes a version importable.
-}
postApiV1AdminCardSetSubmissionDenyR :: ArkhamCardSetSubmissionId -> Handler SubmissionResponse
postApiV1AdminCardSetSubmissionDenyR submissionId = do
  Entity reviewerId _ <- getAdminUser
  row <- runDB $ get404 submissionId
  requirePending row
  DenyPost {denyReason} <- requireCheckJsonBody
  let reason = T.strip denyReason
  -- A denial the author cannot act on is worse than no answer, and this is the
  -- one field only a person can fill in.
  when (T.null reason) $ invalidArgs ["Say why it was turned down"]
  now <- liftIO getCurrentTime

  runDB
    $ P.update
      submissionId
      [ ArkhamCardSetSubmissionStatus P.=. submissionDenied
      , ArkhamCardSetSubmissionReason P.=. Just reason
      , ArkhamCardSetSubmissionReviewedByUserId P.=. Just reviewerId
      , ArkhamCardSetSubmissionReviewedAt P.=. Just now
      , ArkhamCardSetSubmissionUpdatedAt P.=. now
      ]

  updated <- runDB $ get404 submissionId
  response <- submissionResponse (Entity submissionId updated)
  announce response (Just reason)
  pure response

{- | Only a submission nobody has acted on can be acted on.

Two admins opening the queue at once is the ordinary case, and the second one
should be told the decision is already made rather than overwrite it -- which
would silently re-send a different answer to an author who has had one.
-}
requirePending :: ArkhamCardSetSubmission -> Handler ()
requirePending row =
  unless (arkhamCardSetSubmissionStatus row == submissionPending)
    $ invalidArgs ["That submission has already been reviewed"]

{- | Tell the author, if they asked to be told.

Called after the decision is written, and 'sendPlainTextEmail' swallows its own
failures, so nothing here can undo a review.
-}
announce :: SubmissionResponse -> Maybe Text -> Handler ()
announce response reason = when response.submissionResponseNotify do
  token <- getsYesod $ appMailtrapApiToken . appSettings
  sendPlainTextEmail
    token
    response.submissionResponseAuthorEmail
    subject
    "Card Set Submission"
    body
 where
  name = response.submissionResponseSetName
  version = tshow response.submissionResponseVersion
  setUrl =
    "https://arkhamhorror.app/#/card-builder/marketplace/"
      <> toPathPiece response.submissionResponsePublishedCardSetId
  subject = case reason of
    Nothing -> "\"" <> name <> "\" is in the Arkham Horror card marketplace"
    Just _ -> "\"" <> name <> "\" was not approved for the card marketplace"
  body = case reason of
    Nothing ->
      [ "Your card set \"" <> name <> "\" (v" <> version <> ") has been approved."
      , ""
      , "It is in the marketplace now, and anyone can import it:"
      , ""
      , setUrl
      ]
    Just why ->
      [ "Your card set \"" <> name <> "\" (v" <> version <> ") was not approved."
      , ""
      , why
      , ""
      , "You can change the set and submit it again from the card builder:"
      , ""
      , "https://arkhamhorror.app/#/card-builder"
      ]

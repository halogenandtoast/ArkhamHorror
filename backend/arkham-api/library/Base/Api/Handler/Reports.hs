{- | The tracker: filing bugs and feature requests, reading them, and the board
they are worked on.

This replaces a GitHub issue and a GitHub project board. The consequences of
that are what shape the endpoints below:

* A reporter needs no account anywhere else, so filing is a POST here and the
  game export is taken by the server rather than handed to the client to paste
  somewhere.
* Everybody can read the tracker, so a report has a public number, and @hidden@
  exists for whatever a public surface attracts. The writes are rate-limited per
  account, which an issue tracker behind somebody else's login did not have to
  be.
* Nothing syncs. There is no sweep, no mirror and no second copy of the truth,
  which is the whole reason for moving: a status here is the status.

The communication log is one stream of 'ReportEvent's -- replies and column
moves together, oldest first -- so a report reads as a conversation. Status
moves are written by the code that performs them, never reconstructed.

@\/api\/v1\/admin\/...@ is gated on the admin flag by 'Foundation.isAuthorized',
so the admin handlers here do not check it again. They do still need to know
*who* the admin is, for the log.
-}
module Base.Api.Handler.Reports (
  getApiV1ReportsR,
  postApiV1ReportsR,
  getApiV1MyReportsR,
  getApiV1SimilarReportsR,
  getApiV1ReportR,
  postApiV1ReportCommentsR,
  postApiV1ReportSeenR,
  getApiV1AdminReportsR,
  putApiV1AdminReportR,
  deleteApiV1AdminReportR,
  putApiV1AdminReportsOrderR,
  postApiV1AdminReportsImportR,
  deleteApiV1AdminReportEventR,
) where

import Api.Arkham.Export (generateScenarioExport)
import Api.BugUpload (uploadBugExport)
import Data.Aeson qualified as Aeson
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID qualified as UUID
import Database.Esqueleto.Experimental hiding (isNothing)
import Database.Persist qualified as P
import GHC.Records (HasField (..))
import Import hiding (exists, on, update, (=.), (==.), (>=.), (||.))
import Json hiding (Success)
import Network.HTTP.Types (status429)
import UnliftIO.Exception qualified as UnliftIO

-- * What the tracker holds

{- | What a report can be. A third kind would be a change to what the tracker
is, which is why the column carries a CHECK for these two and @status@ does not.
-}
reportKinds :: [Text]
reportKinds = ["bug", "feature"]

{- | The board's columns, in the order the board draws them.

The first six are open work and mirror the GitHub project board this replaced,
including the untriaged column that board expressed as "no status". The last
three are endings: @done@ is fixed or built, @declined@ is "not going to", and
@duplicate@ belongs to a report whose 'reportDuplicateOfId' says which one it is
the same as.

@blocked@ comes last of the six rather than beside @in-progress@: the first five
are a pipeline that a card walks left to right, and a column nothing flows
through sat in the middle of it. Blocked work is parked, not between two stages.
-}
reportStatuses :: [Text]
reportStatuses =
  [ "new"
  , "todo"
  , "in-progress"
  , "ready-for-deploy"
  , "pending-deploy"
  , "blocked"
  , "done"
  , "declined"
  , "duplicate"
  ]

-- | The statuses that mean the report is still live work.
openStatuses :: [Text]
openStatuses = ["new", "todo", "in-progress", "ready-for-deploy", "pending-deploy", "blocked"]

eventComment, eventStatus :: Text
eventComment = "comment"
eventStatus = "status"

-- | How many reports one account may file in a day, and how many replies.
dailyReportLimit, dailyCommentLimit :: Int
dailyReportLimit = 10
dailyCommentLimit = 40

maxTitleLength, maxBodyLength, maxCommentLength :: Int
maxTitleLength = 200
maxBodyLength = 20000
maxCommentLength = 10000

-- | A page of the list. Long enough to skim a column of the board in one.
pageSize :: Int
pageSize = 50

{- | How much of the finished work the board carries.

The columns that are endings only ever grow -- the board this replaced had 456
cards in Done against 44 everywhere else -- and a board is read as a whole or
not at all. The open columns come in full; the closed ones come newest-first and
stop, which is the only part of them anyone looks at.
-}
closedOnBoard :: Int
closedOnBoard = 75

{- | How long an ending stays on the board.

The GitHub project this replaced had a built-in workflow that archived a card a
fortnight after it was closed, and that workflow is the only reason Done was
ever readable there. The same cutoff applies here, and the fortnight is counted
from @updatedAt@ rather than from the move, so a reply on a closed report puts
it back on the board -- a closed report somebody is still talking about is the
one case where an ending is news.

Nothing is archived in the sense of being written down: there is no column, no
sweep and no second copy of the truth, which is what the header of this module
promises. A card that drops off the board is still on the public list under its
status, still at its own number, and still in the author's own list. It has
only stopped being something the board has to carry.
-}
boardArchiveAfter :: NominalDiffTime
boardArchiveAfter = 14 * nominalDay

-- * Wire shapes

{- | A report as any list shows it.

@comments@ and @unread@ are here rather than left to the detail endpoint because
every list wants them: a tracker row that cannot say "three replies, one you
have not read" is a row you have to open to learn anything from.
-}
data ReportRow = ReportRow
  { reportRowId :: ReportId
  , reportRowNumber :: Int
  , reportRowKind :: Text
  , reportRowTitle :: Text
  , reportRowStatus :: Text
  , reportRowAuthor :: Maybe Text
  {- ^ Who to show: the account's name, or the recorded one for an imported
  report. Nothing when neither is left.
  -}
  , reportRowAuthorAccount :: Bool
  -- ^ The name above belongs to an account here, rather than to a GitHub login.
  , reportRowBlocking :: Bool
  , reportRowHidden :: Bool
  , reportRowDuplicateOf :: Maybe Int
  , reportRowComments :: Int
  , reportRowUnread :: Bool
  {- ^ Something has happened since this viewer last looked. False on a report
  they do not follow, which is the difference between news and a tracker they
  have not read.
  -}
  , reportRowFollowing :: Bool
  -- ^ They filed it or replied to it, so changes to it are addressed to them.
  , reportRowGame :: Maybe GameRef
  , reportRowCreatedAt :: UTCTime
  , reportRowUpdatedAt :: UTCTime
  }
  deriving stock Generic

instance ToJSON ReportRow where
  toJSON = genericToJSON $ aesonOptions $ Just "reportRow"
  toEncoding = genericToEncoding $ aesonOptions $ Just "reportRow"

{- | The game a report was filed from, as far as it can still be told.

@step@ is where the game was when the report was written and @currentStep@ is
where it is now, which is the only honest way to say how much has happened
since: a reporter who played on for forty steps has given the fix a moving
target. @id@ and @currentStep@ go empty when the game itself is gone; @name@ and
@step@ were copied onto the report at filing and stay.

@exportUrl@ is admin-only. Every signed-in player can read the tracker, and an
export is the whole game state, including hands nobody at that table chose to
show the rest of the site.
-}
data GameRef = GameRef
  { gameRefId :: Maybe ArkhamGameId
  , gameRefName :: Maybe Text
  , gameRefStep :: Maybe Int
  , gameRefCurrentStep :: Maybe Int
  , gameRefExportUrl :: Maybe Text
  }
  deriving stock Generic

instance ToJSON GameRef where
  toJSON = genericToJSON $ aesonOptions $ Just "gameRef"
  toEncoding = genericToEncoding $ aesonOptions $ Just "gameRef"

-- | One entry of the communication log.
data ReportLogEntry = ReportLogEntry
  { reportLogEntryId :: ReportEventId
  , reportLogEntryKind :: Text
  , reportLogEntryAuthor :: Maybe Text
  , reportLogEntryAuthorAccount :: Bool
  , reportLogEntryAdmin :: Bool
  -- ^ So a reply from whoever is fixing it reads as one.
  , reportLogEntryBody :: Maybe Text
  , reportLogEntryStatus :: Maybe Text
  , reportLogEntryCreatedAt :: UTCTime
  }
  deriving stock Generic

instance ToJSON ReportLogEntry where
  toJSON = genericToJSON $ aesonOptions $ Just "reportLogEntry"
  toEncoding = genericToEncoding $ aesonOptions $ Just "reportLogEntry"

-- | One report in full: the row, its opening post, and its log.
data ReportDetail = ReportDetail
  { reportDetailReport :: ReportRow
  , reportDetailBody :: Text
  , reportDetailLog :: [ReportLogEntry]
  }
  deriving stock Generic

instance ToJSON ReportDetail where
  toJSON = genericToJSON $ aesonOptions $ Just "reportDetail"
  toEncoding = genericToEncoding $ aesonOptions $ Just "reportDetail"

{- | A page of the list.

@total@ is the whole filtered set rather than the page, so the list can say how
many there are without fetching them. The two vocabularies come along so the
filters are drawn from what the server accepts instead of a copy of it.
-}
data ReportsResponse = ReportsResponse
  { reportsResponseReports :: [ReportRow]
  , reportsResponseTotal :: Int
  , reportsResponsePage :: Int
  , reportsResponsePageSize :: Int
  , reportsResponseStatuses :: [Text]
  , reportsResponseKinds :: [Text]
  }
  deriving stock Generic

instance ToJSON ReportsResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "reportsResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "reportsResponse"

{- | The board.

Every card in one list with the column order beside it, rather than an object
per column: an empty column still has to be drawn, and a card that moves then
changes one field instead of moving between two arrays.
-}
data BoardResponse = BoardResponse
  { boardResponseReports :: [ReportRow]
  , boardResponseStatuses :: [Text]
  , boardResponseOpenStatuses :: [Text]
  }
  deriving stock Generic

instance ToJSON BoardResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "boardResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "boardResponse"

-- * Reading

{- | The public list.

Defaults to open reports, newest activity first, because that is the question
the page is for: what is wrong with the site right now. @?status=all@,
@?status=closed@ or a single column narrow it, @?kind=@ and @?q=@ narrow
further, and @?sort=new@ orders by when things were filed instead.

Hidden reports are left out for everyone but an admin. A report's own author
still reaches theirs by its number, which is the link they were given.
-}
getApiV1ReportsR :: Handler ReportsResponse
getApiV1ReportsR = do
  viewer <- getViewer
  kind <- lookupGetParam "kind"
  status <- lookupGetParam "status"
  query <- fmap T.strip <$> lookupGetParam "q"
  ordering <- lookupGetParam "sort"
  page <- max 1 . fromMaybe 1 . (readMaybe . T.unpack =<<) <$> lookupGetParam "page"
  let
    kindFilter r = case kind of
      Just k | k `elem` reportKinds -> [r.kind ==. val k]
      _ -> []
    statusFilter r = case status of
      Just "all" -> []
      Just "closed" -> [not_ (r.status `in_` valList openStatuses)]
      Just s | s `elem` reportStatuses -> [r.status ==. val s]
      _ -> [r.status `in_` valList openStatuses]
    {- Lowered on both sides rather than matched with ILIKE, as the admin user
    search does: somebody looking for a bug types what they remember of it, not
    the case it was written in. -}
    textFilter r = case query of
      Just q
        | not (T.null q) ->
            let fragment = (%) ++. val (T.toLower q) ++. (%)
             in [(lower_ r.title `like` fragment) ||. (lower_ r.body `like` fragment)]
      _ -> []
    visibility r = [r.hidden ==. val False | not viewer.admin]
    conditions r = kindFilter r <> statusFilter r <> textFilter r <> visibility r
    sorting r = case ordering of
      Just "new" -> [desc r.createdAt]
      _ -> [desc r.updatedAt]
  (rows, total) <- runDB do
    rows <- select do
      r <- from $ table @Report
      traverse_ where_ (conditions r)
      orderBy (sorting r)
      limit (fromIntegral pageSize)
      offset (fromIntegral ((page - 1) * pageSize))
      pure r
    counted <- select do
      r <- from $ table @Report
      traverse_ where_ (conditions r)
      pure (countRows @Int)
    pure (rows, maybe 0 unValue (listToMaybe counted))
  reports <- decorate viewer rows
  pure
    ReportsResponse
      { reportsResponseReports = reports
      , reportsResponseTotal = total
      , reportsResponsePage = page
      , reportsResponsePageSize = pageSize
      , reportsResponseStatuses = reportStatuses
      , reportsResponseKinds = reportKinds
      }

{- | The reports this account follows: the ones it filed, and the ones it
replied to.

What the nav badge and the home-page panel are drawn from, so it is ordered by
activity and keeps closed ones. "Your bug is fixed" is the most useful thing the
tracker can tell somebody, and it only reaches them if a report stays on their
list after it is closed.
-}
getApiV1MyReportsR :: Handler [ReportRow]
getApiV1MyReportsR = do
  viewer <- getViewer
  userId <- getRequestUserId
  rows <- runDB $ select do
    r <- from $ table @Report
    where_
      $ (r.authorId ==. val (Just userId))
      ||. exists do
        e <- from $ table @ReportEvent
        where_ $ e.reportId ==. r.id &&. e.userId ==. val (Just userId)
    orderBy [desc r.updatedAt]
    limit (fromIntegral pageSize)
    pure r
  decorate viewer rows

{- | Reports that look like the one somebody is about to file.

Asked for while they type, so that the question a tracker exists to answer --
"has this been said already?" -- is put to them while it can still change what
they do, rather than by a duplicate marker three days later.

Closed reports are in scope, and that is the point: "fixed in a version you are
not on yet" and "we are not going to" are both better answers than filing
again, and neither one is on the open list.

Matched on words rather than on the string, because nobody writes a title the
way the last person wrote it. Candidates are everything sharing a word with
what has been typed; the ranking is done here rather than in SQL, since the
table is hundreds of rows and a scoring expression built per term is a page of
esqueleto to say what one fold says.
-}
getApiV1SimilarReportsR :: Handler [ReportRow]
getApiV1SimilarReportsR = do
  viewer <- getViewer
  typed <- fromMaybe "" <$> lookupGetParam "q"
  let terms = similarityTerms typed
  if null terms
    then pure []
    else do
      candidates <- runDB $ select do
        r <- from $ table @Report
        let
          mentions t =
            let fragment = (%) ++. val t ++. (%)
             in (lower_ r.title `like` fragment) ||. (lower_ r.body `like` fragment)
        where_ $ foldr (\t rest -> mentions t ||. rest) (val False) terms
        traverse_ where_ [r.hidden ==. val False | not viewer.admin]
        orderBy [desc r.updatedAt]
        limit (fromIntegral candidateLimit)
        pure r
      let
        ranked =
          sortOn (Down . fst)
            $ filter ((>= minimumScore) . fst) [(similarity terms (entityVal r), r) | r <- candidates]
      decorate viewer (map snd (take suggestionLimit ranked))

-- | How many rows the ranking reads, and how many of them are offered.
candidateLimit, suggestionLimit :: Int
candidateLimit = 200
suggestionLimit = 5

{- | What a report has to score before it is worth interrupting somebody with.

One word in a title and anything else: one word on its own is a coincidence --
a tracker full of reports called "Test" answers every search that has the word
test anywhere in it -- and two of their words is the point at which two people
are plausibly describing the same thing.
-}
minimumScore :: Int
minimumScore = titleWeight + bodyWeight

{- | A word in the title counts for more than the same word in the body. A
title is what the filer chose to call the thing; a body mentions everything
that happened to be on the table at the time.
-}
titleWeight, bodyWeight :: Int
titleWeight = 3
bodyWeight = 1

{- | The words worth matching on, out of whatever has been typed so far.

Short words and the ones every report here uses carry no signal and would match
the whole tracker, so they go. What is left is deduplicated and capped: a long
description would otherwise build a query with a hundred branches in it.

Everything that is not a letter or a digit becomes a space, which also means no
term can reach LIKE carrying a wildcard of its own.
-}
similarityTerms :: Text -> [Text]
similarityTerms typed =
  take maxTerms
    $ ordNub
    $ filter worthMatching
    $ T.words
    $ T.map (\c -> if isAlphaNum c then c else ' ')
    $ T.toLower
    $ T.take maxTyped typed
 where
  worthMatching word = T.length word >= 4 && word `notElem` tooCommon

maxTerms, maxTyped :: Int
maxTerms = 12
maxTyped = 500

{- | Words that are in half the tracker, so matching on them says nothing: the
ordinary English ones, and the few this whole site is about.
-}
tooCommon :: [Text]
tooCommon =
  [ "that"
  , "this"
  , "with"
  , "when"
  , "from"
  , "have"
  , "been"
  , "they"
  , "them"
  , "then"
  , "than"
  , "there"
  , "their"
  , "would"
  , "could"
  , "should"
  , "about"
  , "after"
  , "before"
  , "which"
  , "while"
  , "where"
  , "because"
  , "game"
  , "games"
  , "card"
  , "cards"
  , "arkham"
  ]

-- | How much a report looks like what is being typed.
similarity :: [Text] -> Report -> Int
similarity terms report =
  sum
    [ if term `T.isInfixOf` title
        then titleWeight
        else
          if term `T.isInfixOf` body
            then bodyWeight
            else 0
    | term <- terms
    ]
 where
  title = T.toLower (reportTitle report)
  body = T.toLower (reportBody report)

-- | One report by its public number, with its communication log.
getApiV1ReportR :: Int -> Handler ReportDetail
getApiV1ReportR number = do
  viewer <- getViewer
  entity <- requireReport viewer number
  detail viewer entity

-- * Filing and replying

data ReportPost = ReportPost
  { reportPostKind :: Text
  , reportPostTitle :: Text
  , reportPostBody :: Text
  , reportPostBlocking :: Bool
  , reportPostGameId :: Maybe ArkhamGameId
  {- ^ The game it is about. Sent rather than inferred, because a report can be
  filed from anywhere, and the export is only taken when there is a game to take
  it from.
  -}
  }

instance FromJSON ReportPost where
  parseJSON = withObject "ReportPost" \o ->
    ReportPost
      <$> o
      .: "kind"
      <*> o
      .: "title"
      <*> o
      .: "body"
      <*> o
      .:? "blocking"
      .!= False
      <*> o
      .:? "gameId"

{- | File a report.

The export is taken here rather than by the client: it is the one part of a
report nobody can retype, and leaving it to a second call means a report filed
without it whenever that call is the one that fails. A game may only be attached
by somebody sitting at it -- an export is the whole game state, and a game id is
not a secret.

An export that will not upload does not lose the report. The reporter has
written the thing we most need; a missing file is a reply asking them how to
reproduce it, not a 500.
-}
postApiV1ReportsR :: Handler ReportDetail
postApiV1ReportsR = do
  userId <- getRequestUserId
  post <- requireCheckJsonBody
  kind <- checkedKind (reportPostKind post)
  title <- checkedText "A report needs a title" maxTitleLength (reportPostTitle post)
  body <- checkedText "A report needs a description" maxBodyLength (reportPostBody post)
  now <- liftIO getCurrentTime
  recent <-
    runDB
      $ P.count
        [ ReportAuthorId P.==. Just userId
        , ReportCreatedAt P.>. addUTCTime (negate nominalDay) now
        ]
  when (recent >= dailyReportLimit)
    $ sendResponseStatus status429 ("That is a lot of reports for one day. Try again tomorrow." :: Text)
  game <- traverse (attachedGame userId) (reportPostGameId post)
  export <- join <$> traverse (uploadExport . fst) game
  (reportId, report) <- runDB do
    {- The number and the insert have to be one transaction, and the lock is
    what stops two reports filed at once from reading the same maximum and
    colliding on UniqueReportNumber. SHARE UPDATE EXCLUSIVE conflicts with
    itself, so filings serialize, but not with the ROW EXCLUSIVE that writes
    take, so triage and replies are not blocked behind somebody filing. -}
    rawExecute "LOCK TABLE reports IN SHARE UPDATE EXCLUSIVE MODE" []
    highest <- select do
      r <- from $ table @Report
      pure (max_ r.number)
    let
      report =
        Report
          { reportNumber = maybe 1 (+ 1) (unValue =<< listToMaybe highest)
          , reportKind = kind
          , reportTitle = title
          , reportBody = body
          , reportStatus = "new"
          , reportAuthorId = Just userId
          , reportAuthorName = Nothing
          , reportBlocking = reportPostBlocking post
          , reportDuplicateOfId = Nothing
          , reportHidden = False
          , reportGameId = fst <$> game
          , reportGameName = snd . snd <$> game
          , reportGameStep = fst . snd <$> game
          , reportExportUrl = export
          , reportPosition = 0
          , reportCreatedAt = now
          , reportUpdatedAt = now
          }
    reportId <- P.insert report
    pure (reportId, report)
  -- Filed is seen: the badge is for what has happened since, and nobody is
  -- behind on their own words.
  markSeen userId reportId now
  viewer <- getViewer
  row <- single viewer (Entity reportId report)
  pure $ ReportDetail row body []

newtype CommentPost = CommentPost {commentPostBody :: Text}

instance FromJSON CommentPost where
  parseJSON = withObject "CommentPost" \o -> CommentPost <$> o .: "body"

{- | Reply to a report.

Anyone signed in may: the tracker is public, and "I have this too, and here is
what I was doing" is the reply that fixes bugs. Replying touches the report,
which puts it back at the top of the list and marks it unread for everyone else
following it.
-}
postApiV1ReportCommentsR :: Int -> Handler ReportDetail
postApiV1ReportCommentsR number = do
  userId <- getRequestUserId
  viewer <- getViewer
  Entity reportId _ <- requireReport viewer number
  post <- requireCheckJsonBody
  body <- checkedText "A reply needs something in it" maxCommentLength (commentPostBody post)
  now <- liftIO getCurrentTime
  recent <-
    runDB
      $ P.count
        [ ReportEventUserId P.==. Just userId
        , ReportEventKind P.==. eventComment
        , ReportEventCreatedAt P.>. addUTCTime (negate nominalDay) now
        ]
  when (recent >= dailyCommentLimit)
    $ sendResponseStatus status429 ("That is a lot of replies for one day. Try again tomorrow." :: Text)
  runDB do
    P.insert_
      ReportEvent
        { reportEventReportId = reportId
        , reportEventUserId = Just userId
        , reportEventAuthorName = Nothing
        , reportEventKind = eventComment
        , reportEventBody = Just body
        , reportEventStatus = Nothing
        , reportEventCreatedAt = now
        }
    P.update reportId [ReportUpdatedAt P.=. now]
  markSeen userId reportId now
  entity' <- runDB $ get404 reportId
  detail viewer (Entity reportId entity')

{- | Mark a report read.

Separate from the detail read because the two answer different things: the page
is opened by anyone, and only the person it is addressed to has a badge to
clear. Clearing it is idempotent.
-}
postApiV1ReportSeenR :: Int -> Handler Aeson.Value
postApiV1ReportSeenR number = do
  userId <- getRequestUserId
  viewer <- getViewer
  Entity reportId _ <- requireReport viewer number
  now <- liftIO getCurrentTime
  markSeen userId reportId now
  pure $ Aeson.object ["seen" Aeson..= True]

-- * The board

{- | The board: every open card, and the last of the finished ones.

One read for the whole thing, because a board is only useful as a whole and the
counts a column shows have to come from the same snapshot as its cards or they
contradict each other. The endings are cut short twice over: 'boardArchiveAfter'
drops the ones nothing has happened to in a fortnight, and 'closedOnBoard' caps
what is left.
-}
getApiV1AdminReportsR :: Handler BoardResponse
getApiV1AdminReportsR = do
  viewer <- getViewer
  now <- liftIO getCurrentTime
  let archivedBefore = addUTCTime (negate boardArchiveAfter) now
  rows <- runDB do
    open <- select do
      r <- from $ table @Report
      where_ $ r.status `in_` valList openStatuses
      orderBy [asc r.position, desc r.createdAt]
      pure r
    closed <- select do
      r <- from $ table @Report
      where_
        $ not_ (r.status `in_` valList openStatuses)
        &&. r.updatedAt >=. val archivedBefore
      orderBy [asc r.position, desc r.updatedAt]
      limit (fromIntegral closedOnBoard)
      pure r
    pure (open <> closed)
  reports <- decorate viewer rows
  pure $ BoardResponse reports reportStatuses openStatuses

data ReportPut = ReportPut
  { reportPutStatus :: Maybe Text
  , reportPutKind :: Maybe Text
  , reportPutDuplicateOf :: Maybe (Maybe Int)
  {- ^ Two layers: the key left out leaves the link alone, and @null@ takes it
  off. Read with '.:!' for the reason "Base.Api.Handler.Notifications" gives.
  -}
  , reportPutHidden :: Maybe Bool
  , reportPutBlocking :: Maybe Bool
  }

instance FromJSON ReportPut where
  parseJSON = withObject "ReportPut" \o ->
    ReportPut
      <$> o
      .:? "status"
      <*> o
      .:? "kind"
      <*> o
      .:! "duplicateOf"
      <*> o
      .:? "hidden"
      <*> o
      .:? "blocking"

{- | Move a card, or change what it is.

A status change writes an event, so the reporter watches it happen in the same
stream as the replies; nothing else here does, because nothing else is addressed
to them. A card moved into a column goes to the top of it, which is where the
thing just decided about belongs.
-}
putApiV1AdminReportR :: ReportId -> Handler ReportRow
putApiV1AdminReportR reportId = do
  adminId <- getRequestUserId
  report <- runDB $ get404 reportId
  edit <- requireCheckJsonBody
  status <- traverse checkedStatus (reportPutStatus edit)
  kind <- traverse checkedKind (reportPutKind edit)
  duplicateOf <- traverse (traverse (resolveDuplicate reportId)) (reportPutDuplicateOf edit)
  now <- liftIO getCurrentTime
  let
    moved = case status of
      Just s | s /= reportStatus report -> Just s
      _ -> Nothing
    changes =
      catMaybes
        [ (ReportStatus P.=.) <$> status
        , (ReportKind P.=.) <$> kind
        , (ReportDuplicateOfId P.=.) <$> duplicateOf
        , (ReportHidden P.=.) <$> reportPutHidden edit
        , (ReportBlocking P.=.) <$> reportPutBlocking edit
        , {- To the top of whatever column it landed in: the reorder endpoint
          writes positions from 1 up, so anything below that is first. -}
          (ReportPosition P.=. 0) <$ moved
        ]
  when (null changes)
    $ invalidArgs ["Nothing to change: send a status, a kind, a duplicateOf, hidden or blocking"]
  updated <- runDB do
    for_ moved \s ->
      P.insert_
        ReportEvent
          { reportEventReportId = reportId
          , reportEventUserId = Just adminId
          , reportEventAuthorName = Nothing
          , reportEventKind = eventStatus
          , reportEventBody = Nothing
          , reportEventStatus = Just s
          , reportEventCreatedAt = now
          }
    P.updateGet reportId ((ReportUpdatedAt P.=. now) : changes)
  markSeen adminId reportId now
  viewer <- getViewer
  single viewer (Entity reportId updated)

data OrderPut = OrderPut
  { orderPutStatus :: Text
  , orderPutIds :: [ReportId]
  }

instance FromJSON OrderPut where
  parseJSON = withObject "OrderPut" \o -> OrderPut <$> o .: "status" <*> o .: "ids"

{- | Reorder one column.

The whole column arrives in order and its positions are written 1..n. Sending
the list rather than one card's new neighbours is what keeps positions from
needing fractions, and makes a reorder idempotent: the same list written twice
is the same column.

Cards named here are also moved into the column, so a drag between columns is
one call. A card that moves writes a status event like any other move; one that
only slides past its neighbours writes nothing, since where a card sits in a
column is not news for the person who filed it.
-}
putApiV1AdminReportsOrderR :: Handler Aeson.Value
putApiV1AdminReportsOrderR = do
  adminId <- getRequestUserId
  order <- requireCheckJsonBody
  status <- checkedStatus (orderPutStatus order)
  now <- liftIO getCurrentTime
  let ids = orderPutIds order
  moved <- runDB do
    known <-
      Map.fromList
        . map (\(Entity k r) -> (k, reportStatus r))
        <$> P.selectList [ReportId P.<-. ids] []
    catMaybes <$> forM (zip [1 :: Int ..] ids) \(index, reportId) ->
      case Map.lookup reportId known of
        Nothing -> pure Nothing
        Just was -> do
          let changed = was /= status
          when changed do
            P.insert_
              ReportEvent
                { reportEventReportId = reportId
                , reportEventUserId = Just adminId
                , reportEventAuthorName = Nothing
                , reportEventKind = eventStatus
                , reportEventBody = Nothing
                , reportEventStatus = Just status
                , reportEventCreatedAt = now
                }
          P.update reportId
            $ (ReportPosition P.=. fromIntegral index)
            : [u | changed, u <- [ReportStatus P.=. status, ReportUpdatedAt P.=. now]]
          pure $ reportId <$ guard changed
  -- Same reason as above: a move is not news for whoever made it.
  for_ moved \reportId -> markSeen adminId reportId now
  pure $ Aeson.object ["ordered" Aeson..= length ids, "moved" Aeson..= length moved]

{- | Delete a report outright.

For spam, not for triage: a report that was read and turned down is @declined@,
which keeps the answer where the person who asked it can find it. 'reportHidden'
is the middle ground. Events and views go with it.
-}
deleteApiV1AdminReportR :: ReportId -> Handler Aeson.Value
deleteApiV1AdminReportR reportId = do
  void $ runDB $ get404 reportId
  runDB $ P.delete reportId
  pure $ Aeson.object ["deleted" Aeson..= reportId]

{- | Delete one entry of a communication log.

Only a reply: a status move is a record of something that happened, and the log
is not a place where that can be made untrue. Taking a reply down leaves the
report's @updatedAt@ alone -- a deletion is not news for anyone following it.
-}
deleteApiV1AdminReportEventR :: ReportEventId -> Handler Aeson.Value
deleteApiV1AdminReportEventR eventId = do
  event <- runDB $ get404 eventId
  unless (reportEventKind event == eventComment)
    $ invalidArgs ["Only a reply can be taken down; a status change is a record of what happened"]
  runDB $ P.delete eventId
  pure $ Aeson.object ["deleted" Aeson..= eventId]

-- * Importing what GitHub already holds

data IssueImport = IssueImport
  { issueImportNumber :: Int
  , issueImportTitle :: Text
  , issueImportBody :: Text
  , issueImportAuthor :: Maybe Text
  , issueImportState :: Text
  -- ^ @OPEN@ or @CLOSED@, as the GitHub API spells it.
  , issueImportStateReason :: Maybe Text
  -- ^ @COMPLETED@, @NOT_PLANNED@ or @DUPLICATE@, for a closed issue.
  , issueImportColumn :: Maybe Text
  -- ^ The project board's Status field, verbatim: @In Progress@ and such.
  , issueImportLabels :: [Text]
  , issueImportCreatedAt :: UTCTime
  , issueImportUpdatedAt :: Maybe UTCTime
  , issueImportComments :: [CommentImport]
  }

instance FromJSON IssueImport where
  parseJSON = withObject "IssueImport" \o ->
    IssueImport
      <$> o
      .: "number"
      <*> o
      .: "title"
      <*> o
      .:? "body"
      .!= ""
      <*> o
      .:? "author"
      <*> o
      .:? "state"
      .!= "OPEN"
      <*> o
      .:? "stateReason"
      <*> o
      .:? "column"
      <*> o
      .:? "labels"
      .!= []
      <*> o
      .: "createdAt"
      <*> o
      .:? "updatedAt"
      <*> o
      .:? "comments"
      .!= []

data CommentImport = CommentImport
  { commentImportAuthor :: Maybe Text
  , commentImportBody :: Text
  , commentImportCreatedAt :: UTCTime
  }

instance FromJSON CommentImport where
  parseJSON = withObject "CommentImport" \o ->
    CommentImport <$> o .:? "author" <*> o .:? "body" .!= "" <*> o .: "createdAt"

newtype ImportPost = ImportPost {importPostIssues :: [IssueImport]}

instance FromJSON ImportPost where
  parseJSON = withObject "ImportPost" \o -> ImportPost <$> o .: "issues"

{- | Take the GitHub issues that are still open and make them reports.

Fed by @scripts/export-github-issues.sh@, which is the only part of this that
needs a GitHub token: the import itself is an ordinary admin write, so it can be
run against whichever environment the admin is signed in to without anything
here holding a credential.

Each issue keeps its number, so @#61@ goes on meaning what it meant, and new
reports are numbered above the highest imported one. Authorship cannot be kept
-- a GitHub login is not an account here -- so the reporter is recorded in the
text and 'reportAuthorId' is left empty, which also means an imported report has
nobody to send badges to. Replies come across the same way.

Idempotent by number: a second run adds what is new and leaves the rest, so an
import that half-failed can simply be run again.
-}
postApiV1AdminReportsImportR :: Handler Aeson.Value
postApiV1AdminReportsImportR = do
  post <- requireCheckJsonBody
  existing <-
    runDB
      $ map (reportNumber . entityVal)
      <$> P.selectList [] [P.Asc ReportNumber]
  let fresh = filter ((`notElem` existing) . issueImportNumber) (importPostIssues post)
  imported <- forM fresh \issue -> do
    gameId <- resolveImportedGame (issueImportBody issue)
    let
      report =
        Report
          { reportNumber = issueImportNumber issue
          , reportKind = importedKind issue
          , reportTitle = T.take maxTitleLength (issueImportTitle issue)
          , reportBody = T.take maxBodyLength (importedBody issue)
          , reportStatus = importedStatus issue
          , reportAuthorId = Nothing
          , reportAuthorName = issueImportAuthor issue
          , reportBlocking = "blocks game" `elem` map T.toLower (issueImportLabels issue)
          , reportDuplicateOfId = Nothing
          , reportHidden = False
          , reportGameId = gameId
          , reportGameName = Nothing
          , reportGameStep = Nothing
          , reportExportUrl = lineValue "file:" (issueImportBody issue)
          , reportPosition = 0
          , reportCreatedAt = issueImportCreatedAt issue
          , reportUpdatedAt = fromMaybe (issueImportCreatedAt issue) (issueImportUpdatedAt issue)
          }
    runDB do
      reportId <- P.insert report
      for_ (issueImportComments issue) \c ->
        P.insert_
          ReportEvent
            { reportEventReportId = reportId
            , reportEventUserId = Nothing
            , reportEventAuthorName = commentImportAuthor c
            , reportEventKind = eventComment
            , reportEventBody = Just (T.take maxCommentLength (commentImportBody c))
            , reportEventStatus = Nothing
            , reportEventCreatedAt = commentImportCreatedAt c
            }
    pure (issueImportNumber issue)
  pure
    $ Aeson.object
      [ "imported" Aeson..= imported
      , "skipped" Aeson..= (length (importPostIssues post) - length imported)
      ]

-- | A feature request if it was labelled as one, otherwise a bug.
importedKind :: IssueImport -> Text
importedKind issue
  | any (`elem` ["enhancement", "feature", "feature request"]) labels = "feature"
  | otherwise = "bug"
 where
  labels = map T.toLower (issueImportLabels issue)

{- | Where an imported issue lands.

A closed issue goes to the ending its close reason names. An open one takes its
board column, or @new@ when it had none -- which is what "no status" on that
board meant.
-}
importedStatus :: IssueImport -> Text
importedStatus issue
  | T.toUpper (issueImportState issue) == "CLOSED" = case T.toUpper <$> issueImportStateReason issue of
      Just "NOT_PLANNED" -> "declined"
      Just "DUPLICATE" -> "duplicate"
      _ -> "done"
  | otherwise = fromMaybe "new" (columnStatus =<< issueImportColumn issue)

-- | A project board column spelled as a status here.
columnStatus :: Text -> Maybe Text
columnStatus column =
  let slug = T.replace " " "-" (T.toLower (T.strip column))
   in slug <$ guard (slug `elem` reportStatuses)

{- | The issue's text, with a line saying where it came from.

The old template wrote @game:@ and @file:@ lines into the body, and they are
left in: the game link is worth keeping readable, and the attribution line is
the only remaining record of who filed it.
-}
importedBody :: IssueImport -> Text
importedBody issue =
  T.strip (issueImportBody issue)
    <> "\n\nImported from GitHub issue #"
    <> tshow (issueImportNumber issue)
    <> "."

{- | The game an imported issue was filed from, if it is still here.

The URL of the game the reporter was in was written into the body as a @game:@
line. Only set when that game still exists: a dangling reference would not pass
the foreign key, and a game from two years ago usually does not.
-}
resolveImportedGame :: Text -> Handler (Maybe ArkhamGameId)
resolveImportedGame body = case lineValue "game:" body >>= gameIdFromUrl of
  Nothing -> pure Nothing
  Just gameId -> do
    found <- runDB $ P.get gameId
    pure $ gameId <$ found

{- | The game id in a game URL.

Every piece of the URL is tried rather than the last path segment, because the
links in these bodies are not all the same shape: the app routes on a fragment,
so a game is @\/#\/games\/<id>@, and some of them arrived with a tracking query
in front of that. A UUID anywhere in the URL is the id.
-}
gameIdFromUrl :: Text -> Maybe ArkhamGameId
gameIdFromUrl url =
  fmap ArkhamGameKey
    $ listToMaybe
    $ mapMaybe UUID.fromText
    $ T.split (`elem` ['/', '?', '#', '&', '=']) url

-- | The value of a @prefix: value@ line, wherever it sits in a body.
lineValue :: Text -> Text -> Maybe Text
lineValue prefix body =
  fmap (T.strip . T.drop (T.length prefix))
    $ find (T.isPrefixOf prefix)
    $ map T.stripStart
    $ T.lines body

-- * Shared pieces

{- | Who is asking, and whether they are an admin.

Every read here is behind a signed-in account -- the site is invite-only, so
there is no anonymous reader to serve -- but the viewer is carried around rather
than re-read, because three separate things depend on it: what is visible, what
is unread, and whether the export link comes along.
-}
data Viewer = Viewer
  { viewerUserId :: UserId
  , viewerAdmin :: Bool
  }

instance HasField "admin" Viewer Bool where
  getField = viewerAdmin

instance HasField "userId" Viewer UserId where
  getField = viewerUserId

getViewer :: Handler Viewer
getViewer = do
  userId <- getRequestUserId
  user <- runDB $ P.get userId
  pure $ Viewer userId (maybe False userAdmin user)

{- | A report by its public number.

A hidden one is reachable by its author and by an admin, and is a 404 to
everyone else: the number is in a URL somebody was given, and a 403 would tell
them the report exists.
-}
requireReport :: Viewer -> Int -> Handler (Entity Report)
requireReport viewer number = do
  mReport <- runDB $ P.getBy (UniqueReportNumber number)
  case mReport of
    Nothing -> notFound
    Just entity@(Entity _ report)
      | not (reportHidden report) -> pure entity
      | viewer.admin -> pure entity
      | reportAuthorId report == Just viewer.userId -> pure entity
      | otherwise -> notFound

{- | The game to attach, and what to copy off it.

Only a game this account has a seat at: an export is the whole game state, and a
game id travels in URLs. The name and step are read now and stored, because the
report has to go on saying what it was about once the game is deleted.
-}
attachedGame :: UserId -> ArkhamGameId -> Handler (ArkhamGameId, (Int, Text))
attachedGame userId gameId = do
  rows <- runDB do
    void $ getBy404 (UniquePlayer userId gameId)
    {- Projected rather than fetched: `current_data` is a TOASTed blob of tens of
    kilobytes and none of it is wanted here. -}
    select do
      g <- from $ table @ArkhamGame
      where_ $ g.id ==. val gameId
      pure (g.step, g.name)
  case rows of
    [] -> notFound
    (Value step, Value name) : _ -> pure (gameId, (step, name))

{- | Upload the game's export, or give back nothing.

Deliberately swallows the failure -- see 'postApiV1ReportsR'. The export is the
best evidence a report can carry and still not worth the report itself.
-}
uploadExport :: ArkhamGameId -> Handler (Maybe Text)
uploadExport gameId = do
  export <- generateScenarioExport gameId
  result <- UnliftIO.tryAny $ uploadBugExport "" (encode export)
  case result of
    Right url -> pure (Just url)
    Left err -> do
      putStrLn
        $ "could not upload a bug export for "
        <> show gameId
        <> ": "
        <> show err
      pure Nothing

-- | A report with everything its page shows.
detail :: Viewer -> Entity Report -> Handler ReportDetail
detail viewer entity@(Entity reportId report) = do
  row <- single viewer entity
  log' <- reportLog reportId
  pure $ ReportDetail row (reportBody report) log'

-- | The communication log, oldest first, which is how a conversation reads.
reportLog :: ReportId -> Handler [ReportLogEntry]
reportLog reportId = do
  rows <- runDB $ select do
    (e :& u) <-
      from
        $ table @ReportEvent
        `leftJoin` table @User
          `on` (\(e :& u) -> e.userId ==. u.id)
    where_ $ e.reportId ==. val reportId
    orderBy [asc e.createdAt]
    pure (e, u)
  pure
    [ ReportLogEntry
        { reportLogEntryId = entityKey e
        , reportLogEntryKind = reportEventKind (entityVal e)
        , reportLogEntryAuthor =
            (userUsername . entityVal <$> u) <|> reportEventAuthorName (entityVal e)
        , reportLogEntryAuthorAccount = isJust u
        , reportLogEntryAdmin = maybe False (userAdmin . entityVal) u
        , reportLogEntryBody = reportEventBody (entityVal e)
        , reportLogEntryStatus = reportEventStatus (entityVal e)
        , reportLogEntryCreatedAt = reportEventCreatedAt (entityVal e)
        }
    | (e, u) <- rows
    ]

-- | One report, decorated the way a list decorates it.
single :: Viewer -> Entity Report -> Handler ReportRow
single viewer entity = do
  rows <- decorate viewer [entity]
  case rows of
    row : _ -> pure row
    [] -> notFound

{- | Turn report rows into what the wire wants: author names, reply counts,
unread marks, and the state of the game each one came from.

Everything is looked up for the whole page at once rather than per row. A
tracker page is fifty reports, and fifty of anything per row is how a list page
becomes the slowest thing on a site.
-}
decorate :: Viewer -> [Entity Report] -> Handler [ReportRow]
decorate _ [] = pure []
decorate viewer entities = do
  let
    reports = map entityVal entities
    ids = map entityKey entities
    authorIds = ordNub $ mapMaybe reportAuthorId reports
    gameIds = ordNub $ mapMaybe reportGameId reports
    duplicateIds = ordNub $ mapMaybe reportDuplicateOfId reports
  (authors, counts, views, steps, duplicates, followed) <- runDB do
    authors <-
      Map.fromList
        . map (\(Entity k u) -> (k, userUsername u))
        <$> P.selectList [UserId P.<-. authorIds] []
    counts <-
      Map.fromList
        . map (\(Value k, Value n) -> (k, n))
        <$> select do
          e <- from $ table @ReportEvent
          where_ $ e.reportId `in_` valList ids &&. e.kind ==. val eventComment
          groupBy e.reportId
          pure (e.reportId, countRows @Int)
    views <-
      Map.fromList
        . map (\(Entity _ v) -> (reportViewReportId v, reportViewViewedAt v))
        <$> P.selectList
          [ReportViewUserId P.==. viewer.userId, ReportViewReportId P.<-. ids]
          []
    {- The step each linked game is on now. Projected, so the games' blobs are
    left where they are. -}
    steps <-
      Map.fromList
        . map (\(Value k, Value s) -> (k, s))
        <$> select do
          g <- from $ table @ArkhamGame
          where_ $ g.id `in_` valList gameIds
          pure (g.id, g.step)
    duplicates <-
      Map.fromList
        . map (\(Entity k r) -> (k, reportNumber r))
        <$> P.selectList [ReportId P.<-. duplicateIds] []
    {- Followed means addressed to you: you filed it, or you have replied to it.
    Only then can there be news on it. -}
    followed <-
      map unValue <$> select do
        e <- from $ table @ReportEvent
        where_ $ e.reportId `in_` valList ids &&. e.userId ==. val (Just viewer.userId)
        groupBy e.reportId
        pure e.reportId
    pure (authors, counts, views, steps, duplicates, followed)
  pure
    [ let report = entityVal entity
          reportId = entityKey entity
          following =
            reportAuthorId report == Just viewer.userId || reportId `elem` followed
       in ReportRow
            { reportRowId = reportId
            , reportRowNumber = reportNumber report
            , reportRowKind = reportKind report
            , reportRowTitle = reportTitle report
            , reportRowStatus = reportStatus report
            , reportRowAuthor =
                ((`Map.lookup` authors) =<< reportAuthorId report)
                  <|> reportAuthorName report
            , reportRowAuthorAccount = isJust (reportAuthorId report)
            , reportRowBlocking = reportBlocking report
            , reportRowHidden = reportHidden report
            , reportRowDuplicateOf = (`Map.lookup` duplicates) =<< reportDuplicateOfId report
            , reportRowComments = fromMaybe 0 (Map.lookup reportId counts)
            , reportRowUnread =
                following && maybe True (< reportUpdatedAt report) (Map.lookup reportId views)
            , reportRowFollowing = following
            , reportRowGame = gameOf viewer steps report
            , reportRowCreatedAt = reportCreatedAt report
            , reportRowUpdatedAt = reportUpdatedAt report
            }
    | entity <- entities
    ]

{- | What is left to say about the game a report came from.

Nothing at all when it was filed without one. The step it was filed at survives
the game, so a report whose game has been deleted still says where it happened.
-}
gameOf :: Viewer -> Map ArkhamGameId Int -> Report -> Maybe GameRef
gameOf viewer steps report
  | isNothing (reportGameId report)
  , isNothing (reportGameName report)
  , isNothing (reportGameStep report)
  , isNothing (reportExportUrl report) =
      Nothing
  | otherwise =
      Just
        GameRef
          { gameRefId = reportGameId report
          , gameRefName = reportGameName report
          , gameRefStep = reportGameStep report
          , gameRefCurrentStep = (`Map.lookup` steps) =<< reportGameId report
          , gameRefExportUrl = if viewer.admin then reportExportUrl report else Nothing
          }

-- | Record that this account has seen the report as it stands.
markSeen :: UserId -> ReportId -> UTCTime -> Handler ()
markSeen userId reportId now =
  runDB
    $ void
    $ P.upsertBy
      (UniqueReportView reportId userId)
      ReportView
        { reportViewReportId = reportId
        , reportViewUserId = userId
        , reportViewViewedAt = now
        }
      [ReportViewViewedAt P.=. now]

checkedKind :: Text -> Handler Text
checkedKind raw = do
  let kind = T.toLower (T.strip raw)
  unless (kind `elem` reportKinds)
    $ invalidArgs ["Unknown kind: " <> kind <> ". Use one of " <> T.intercalate ", " reportKinds]
  pure kind

checkedStatus :: Text -> Handler Text
checkedStatus raw = do
  let status = T.toLower (T.strip raw)
  unless (status `elem` reportStatuses)
    $ invalidArgs ["Unknown status: " <> status <> ". Use one of " <> T.intercalate ", " reportStatuses]
  pure status

-- | The report a @duplicateOf@ number names, which cannot be the report itself.
resolveDuplicate :: ReportId -> Int -> Handler ReportId
resolveDuplicate reportId number = do
  mReport <- runDB $ P.getBy (UniqueReportNumber number)
  case mReport of
    Nothing -> invalidArgs ["There is no report #" <> tshow number]
    Just (Entity otherId _)
      | otherId == reportId -> invalidArgs ["A report cannot be a duplicate of itself"]
      | otherwise -> pure otherId

checkedText :: Text -> Int -> Text -> Handler Text
checkedText missing longest raw = do
  let text = T.strip raw
  when (T.null text) $ invalidArgs [missing]
  when (T.length text > longest) $ invalidArgs ["That is too long"]
  pure text

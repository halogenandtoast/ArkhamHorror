{- | What the archive sweep has actually reclaimed.

The whole mechanism is a bet that most of @arkham_steps@ belongs to games nobody
is coming back to, so the numbers that matter are: how many games are offloaded,
how many step rows that removed, how much is in the bucket, and how deep the
remaining backlog is. Without them there is no way to tell a sweep that is
working from one that silently stopped.

Counts come off @arkham_game_archives@, which has one narrow row per archived
game, so they are read live rather than from a materialized view like the play
statistics next door. The backlog is the exception: it is a predicate over the
games table, so it is asked for with a cap rather than exactly.
-}
module Api.Handler.Arkham.Admin.Archives (
  getApiV1AdminArchivesR,
  postApiV1AdminArchivesRunR,
  postApiV1AdminArchiveGameR,
) where

import Api.Arkham.Archive
import Control.Concurrent (forkIO)
import Data.Aeson qualified as Aeson
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Database.Persist qualified as P

-- 'Value' is esqueleto's here; aeson's is reached through the qualified import.
import Import hiding (Value, on, (==.), (>.))
import Json hiding (Value)
import Network.HTTP.Types.Status qualified as Status

-- | How far ahead the backlog is counted. See 'archiveCandidateCount'.
candidateCap :: Int
candidateCap = 1000

data ArchiveRow = ArchiveRow
  { archiveRowGameId :: ArkhamGameId
  , archiveRowGameName :: Text
  , archiveRowState :: ArchiveState
  , archiveRowSteps :: Int
  , archiveRowBytes :: Int
  , archiveRowArchivedAt :: UTCTime
  , archiveRowGameUpdatedAt :: UTCTime
  , archiveRowRestoredAt :: Maybe UTCTime
  , archiveRowError :: Maybe Text
  }
  deriving stock Generic

instance ToJSON ArchiveRow where
  toJSON = genericToJSON $ aesonOptions $ Just "archiveRow"
  toEncoding = genericToEncoding $ aesonOptions $ Just "archiveRow"

data ArchiveStatsResponse = ArchiveStatsResponse
  { archiveStatsResponseEnabled :: Bool
  {- ^ Whether a bucket is configured. Everything else is zero when it is not,
  and a panel of zeros should say why.
  -}
  , archiveStatsResponseSweepEnabled :: Bool
  {- ^ Whether the daily job runs, which is a separate question. Archiving by
  hand works either way, and the panel has to say which it is -- "nothing has
  been archived" means something different when nothing is going to be.
  -}
  , archiveStatsResponseBucket :: Maybe Text
  , archiveStatsResponseAfterMonths :: Int
  , archiveStatsResponseBatchSize :: Int
  , archiveStatsResponsePauseSeconds :: Int
  , archiveStatsResponseArchived :: Int
  , archiveStatsResponseRestoring :: Int
  , archiveStatsResponseRestored :: Int
  , archiveStatsResponseFailed :: Int
  , archiveStatsResponseStepsOffloaded :: Int
  -- ^ Step rows not in the database right now because they are in the bucket.
  , archiveStatsResponseStepsEverArchived :: Int
  {- ^ Including the ones that have since been restored, which is the measure of
  how often this gets asked for rather than of what it saved.
  -}
  , archiveStatsResponseBytesStored :: Int
  , archiveStatsResponseArchivedLastDay :: Int
  , archiveStatsResponseRestoredLastWeek :: Int
  , archiveStatsResponseCandidates :: Int
  , archiveStatsResponseCandidateCap :: Int
  {- ^ The count above stops here, so @candidates == candidateCap@ means "at
  least this many" rather than "exactly".
  -}
  , archiveStatsResponseLastSweepAt :: Maybe UTCTime
  , archiveStatsResponseLastSweepMs :: Maybe Int
  , archiveStatsResponseRecent :: [ArchiveRow]
  }
  deriving stock Generic

instance ToJSON ArchiveStatsResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "archiveStatsResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "archiveStatsResponse"

-- | Per-state totals in one pass, folded in Haskell rather than pivoted in SQL.
stateTotals :: DB [(ArchiveState, Int, Int, Int)]
stateTotals = do
  rows <- select do
    a <- from $ table @ArkhamGameArchive
    groupBy a.state
    pure
      ( a.state
      , countRows :: SqlExpr (Value Int)
      , sum_ a.steps :: SqlExpr (Value (Maybe Int))
      , sum_ a.bytes :: SqlExpr (Value (Maybe Int))
      )
  pure
    [ (st, n, fromMaybe 0 steps, fromMaybe 0 bytes)
    | (Value st, Value n, Value steps, Value bytes) <- rows
    ]

countSince :: (SqlExpr (Entity ArkhamGameArchive) -> SqlExpr (Value Bool)) -> DB Int
countSince predicate = do
  rows <- select do
    a <- from $ table @ArkhamGameArchive
    where_ $ predicate a
    pure (countRows :: SqlExpr (Value Int))
  pure $ maybe 0 unValue (listToMaybe rows)

{- | The most recent archives, newest first.

Joined to the game for its name only. Selecting the game row itself would pull
@current_data@ -- a 28kB jsonb blob per row, detoasted to be thrown away -- which
is the sort of thing this whole feature exists to stop doing.
-}
recentArchives :: DB [ArchiveRow]
recentArchives = do
  rows <- select do
    (a :& g) <-
      from
        $ table @ArkhamGameArchive
        `innerJoin` table @ArkhamGame
          `on` (\(a :& g) -> a.arkhamGameId ==. g.id)
    orderBy [desc a.archivedAt]
    limit 25
    pure (a, g.name)
  pure
    [ ArchiveRow
        { archiveRowGameId = archive.arkhamGameId
        , archiveRowGameName = name
        , archiveRowState = archive.state
        , archiveRowSteps = archive.steps
        , archiveRowBytes = archive.bytes
        , archiveRowArchivedAt = archive.archivedAt
        , archiveRowGameUpdatedAt = archive.gameUpdatedAt
        , archiveRowRestoredAt = archive.restoredAt
        , archiveRowError = archive.errorMessage
        }
    | (Entity _ archive, Value name) <- rows
    ]

getApiV1AdminArchivesR :: Handler ArchiveStatsResponse
getApiV1AdminArchivesR = do
  settings <- getsApp appSettings
  now <- liftIO getCurrentTime
  let
    mConfig = archiveConfig settings
    cutoff = archiveCutoff (appArchiveAfterMonths settings) now
    dayAgo = addUTCTime (-86400) now
    weekAgo = addUTCTime (-604800) now

  totals <- runDB stateTotals
  archivedLastDay <- runDB $ countSince \a -> a.archivedAt >. val dayAgo
  restoredLastWeek <- runDB $ countSince \a -> a.restoredAt >. just (val weekAgo)
  -- Only worth asking when there is somewhere to put them.
  candidates <- case mConfig of
    Nothing -> pure 0
    Just _ -> runDB $ archiveCandidateCount cutoff candidateCap
  recent <- runDB recentArchives
  lastSweep <- runDB $ P.getBy (UniqueStatsRefreshName archiveSweepName)

  let
    countFor st = sum [n | (s, n, _, _) <- totals, s == st]
    stepsFor st = sum [steps | (s, _, steps, _) <- totals, s == st]

  pure
    ArchiveStatsResponse
      { archiveStatsResponseEnabled = isJust mConfig
      , archiveStatsResponseSweepEnabled = appArchiveSweepEnabled settings
      , archiveStatsResponseBucket = appArchiveBucket settings
      , archiveStatsResponseAfterMonths = appArchiveAfterMonths settings
      , archiveStatsResponseBatchSize = appArchiveBatchSize settings
      , archiveStatsResponsePauseSeconds = appArchiveBatchPauseSeconds settings
      , archiveStatsResponseArchived = countFor Archived
      , archiveStatsResponseRestoring = countFor Restoring
      , archiveStatsResponseRestored = countFor Restored
      , archiveStatsResponseFailed = countFor Failed
      , archiveStatsResponseStepsOffloaded =
          stepsFor Archived + stepsFor Restoring + stepsFor Failed
      , archiveStatsResponseStepsEverArchived = sum [steps | (_, _, steps, _) <- totals]
      , -- One live object per row: a re-archive deletes the one it replaces, and a
        -- restore leaves the object where it is.
        archiveStatsResponseBytesStored = sum [bytes | (_, _, _, bytes) <- totals]
      , archiveStatsResponseArchivedLastDay = archivedLastDay
      , archiveStatsResponseRestoredLastWeek = restoredLastWeek
      , archiveStatsResponseCandidates = candidates
      , archiveStatsResponseCandidateCap = candidateCap
      , archiveStatsResponseLastSweepAt = arkhamStatsRefreshRefreshedAt . entityVal <$> lastSweep
      , archiveStatsResponseLastSweepMs = arkhamStatsRefreshDurationMs . entityVal <$> lastSweep
      , archiveStatsResponseRecent = recent
      }

{- | Start a sweep now.

Mostly unnecessary: 'archiveSweeper' claims a run a minute after every replica
starts, so turning the feature on or changing the throttle is a deploy and the
run follows by itself. This is for the times in between -- and for watching one
go.

Claims the same row the scheduler does, with a one-hour window instead of
twenty, because a button that can be pressed twice is a button that will be.
@?force=true@ skips the claim for the case where a run is genuinely wanted
again: two overlapping sweeps are wasteful rather than harmful (they would
re-export a game the other has already archived), but they are not something to
do by accident.

Returns as soon as it has started. A run now drains the whole backlog, which is
hours, not a request.
-}
postApiV1AdminArchivesRunR :: Handler Aeson.Value
postApiV1AdminArchivesRunR = do
  settings <- getsApp appSettings
  cfg <- maybe notFound pure (archiveConfig settings)
  app <- getApp
  -- Not `force`: that name is taken by Control.DeepSeq, via Import.
  forced <- (== Just "true") <$> lookupGetParam "force"
  claimed <- if forced then pure True else runDB (claimSweep 1)
  if not claimed
    then
      sendStatusJSON Status.status409
        $ object
          -- 'force' tells the client it may offer to repeat with ?force=true.
          [ "error" .= ("A sweep has already run in the last hour" :: Text)
          , "force" .= True
          ]
    else do
      liftIO $ void $ forkIO $ void $ runAndRecordSweep app cfg
      sendStatusJSON Status.status202 $ object ["started" .= True, "forced" .= forced]

{- | Archive one game, named by whoever is asking.

The way to do this the first time. A sweep picks its own games by age, which is
no use when what you want is to archive a game you have chosen, press Undo on
it, and watch the history come back -- on one game, with the object intact
either way, before a job is allowed near twenty thousand of them.

Deliberately not subject to the retention cutoff. It does refuse a game played
in the last day unless asked twice (@?force=true@): the guard is not against
the operation, which is reversible, but against a mistyped id landing on
somebody's live table.

Synchronous, because the answer is the point. For a typical game that is a
couple of seconds; for the largest it can exceed the proxy's timeout, in which
case the work still finishes and the row appears in the panel on refresh.
-}
postApiV1AdminArchiveGameR :: ArkhamGameId -> Handler Aeson.Value
postApiV1AdminArchiveGameR gameId = do
  settings <- getsApp appSettings
  cfg <- maybe notFound pure (archiveConfig settings)
  app <- getApp
  forced <- (== Just "true") <$> lookupGetParam "force"
  now <- liftIO getCurrentTime

  -- Narrow columns on purpose: `current_data` is a 28kB blob with nothing in it
  -- this needs, and detoasting it is the habit this feature exists to break.
  mGame <- runDB $ selectOne do
    g <- from $ table @ArkhamGame
    where_ $ g.id ==. val gameId
    pure (g.name, g.updatedAt)
  (Value name, Value updatedAt) <- maybe notFound pure mGame

  runDB (lookupArchive gameId) >>= traverse_ \existing ->
    when (existing.state /= Restored)
      $ sendStatusJSON Status.status409
      $ object
        [ "error" .= ("This game's history is already offloaded" :: Text)
        , "state" .= existing.state
        ]

  let hoursIdle = diffUTCTime now updatedAt / 3600
  unless (forced || hoursIdle >= 24)
    $ sendStatusJSON Status.status409
    $ object
      [ "error"
          .= ( "This game was played "
                 <> tshow (round hoursIdle :: Int)
                 <> "h ago. Pass force=true to archive it anyway."
             )
      , "game" .= name
      , "updatedAt" .= updatedAt
      ]

  liftIO (archiveOne app cfg gameId) >>= \case
    Right outcome ->
      pure
        $ object
          [ "archived" .= True
          , "game" .= name
          , "steps" .= archiveOutcomeSteps outcome
          , "bytes" .= archiveOutcomeBytes outcome
          , "key" .= archiveOutcomeKey outcome
          ]
    -- Nothing was deleted: 'archiveGame' only deletes after the upload has been
    -- read back and checked. The message is the operational detail -- usually
    -- the bucket or the credentials -- and it is what the panel shows.
    Left err ->
      sendStatusJSON Status.status500
        $ object ["archived" .= False, "game" .= name, "error" .= err]

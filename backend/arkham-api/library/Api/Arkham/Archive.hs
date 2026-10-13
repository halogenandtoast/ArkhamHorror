{- | Offloading a game's undo history to object storage, and bringing it back.

@arkham_steps@ is the database. It is 94 of 98 GB, ~620 rows per game, and
nothing has ever pruned it: the only delete in the application removes steps
/above/ the one a game is sitting on, so a finished campaign keeps every patch
it ever generated forever, against the possibility of an undo nobody is going
to ask for.

So: a game nobody has touched in months has its whole step history written to S3
as the same debug export the full-export endpoint serves, and the rows are
deleted. The game stays playable -- only undo is affected -- and the first player
to press undo is offered the history back.

__Nothing here decodes game data.__ Steps are read and written as raw @jsonb@ and
the game blob comes from 'ArkhamGameRaw', so no 'Game' and no 'Message' is ever
parsed. This is not an optimisation. The games most worth archiving are the
oldest ones, and an engine that has moved on for six months may well not be able
to decode them any more; a sweep built on the typed entities would fail on
exactly the rows it is there to collect, and a restore would be unable to put
back what it had successfully stored.

__The upload is confirmed before anything is deleted.__ 'archiveGame' does a
HEAD on the object it just wrote, and only then deletes the rows. The whole
point is that the data still exists afterwards.
-}
module Api.Arkham.Archive (
  -- * Configuration
  ArchiveConfig (..),
  archiveConfig,
  archiveCutoff,

  -- * Archiving
  ArchiveOutcome (..),
  archiveGame,
  archiveOne,
  archiveCandidates,
  archiveCandidateCount,

  -- * Restoring
  claimForRestore,
  restoreGame,
  startRestore,
  restoreWorker,

  -- * The daily sweep
  ArchiveSweep (..),
  archiveSweeper,
  runArchiveSweep,
  claimSweep,
  recordSweep,
  runAndRecordSweep,
  archiveSweepName,

  -- * Reads
  lookupArchive,
) where

-- Explicit lists: both modules export record field names (ChunkedBody's
-- 'length', GetObjectResponse's 'body') that otherwise shadow the prelude.
import Amazonka (Env, discover, hashedFile, newEnv, runResourceT, send, sinkBody, toBody)
import Amazonka.S3 (
  BucketName (..),
  ObjectKey (..),
  newDeleteObject,
  newGetObject,
  newHeadObject,
  newPutObject,
 )
import Amazonka.S3.GetObject (getObjectResponse_body)
import Amazonka.S3.PutObject (putObject_contentType)
import Api.Arkham.Helpers (ApiResponse (..), publishFromApp)
import Codec.Compression.GZip qualified as GZip
import Conduit (sinkFile)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (view, (?~))
import Control.Monad (foldM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.List.Extra (chunksOf)
import Data.Text qualified as T
import Data.Time.Calendar (addGregorianMonthsClip)
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Traversable (for)
import Database.Esqueleto.Experimental
import Database.Persist qualified as P
import Entity.Arkham.GameRaw
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, exists, on, update, (!=.), (<.), (=.), (==.), (>.), (>=.))
import System.Directory (getFileSize, getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import UnliftIO.Exception (bracket, tryAny)

-- ------------------------------------------------------------------ config ---

data ArchiveConfig = ArchiveConfig
  { archiveConfigBucket :: BucketName
  , archiveConfigAfterMonths :: Int
  , archiveConfigBatchSize :: Int
  , archiveConfigPauseSeconds :: Int
  }

{- | The configuration, or 'Nothing' when no bucket is set.

'Nothing' is the whole feature off, which is what development gets: no sweep, no
uploads, and no endpoint. There is deliberately no default bucket name -- a
deployment that has not been told where to put the data has no business deleting
it.
-}
archiveConfig :: AppSettings -> Maybe ArchiveConfig
archiveConfig settings = do
  bucket <- appArchiveBucket settings
  pure
    ArchiveConfig
      { archiveConfigBucket = BucketName bucket
      , archiveConfigAfterMonths = max 1 (appArchiveAfterMonths settings)
      , archiveConfigBatchSize = max 1 (appArchiveBatchSize settings)
      , archiveConfigPauseSeconds = max 0 (appArchiveBatchPauseSeconds settings)
      }

-- | How often the sweeper wakes up to see whether today's run has happened.
sweepPollSeconds :: Int
sweepPollSeconds = 3600

{- | A run is claimed at most once in this window, across every replica.

Twenty rather than twenty-four: the poll is hourly, and a 24-hour guard would
drift the run an hour later each day until it skipped one.
-}
sweepIntervalHours :: Int
sweepIntervalHours = 20

{- | How long after boot the first claim is attempted.

A run starts at startup rather than at the first hourly tick, so deploying is
how a sweep is kicked off -- there is nothing to wait for and nothing to press.
The delay is only so the pod finishes coming up first; the claim means that at
most one replica of a rollout proceeds, and only if today's run has not happened.
-}
bootDelaySeconds :: Int
bootDelaySeconds = 60

{- | Give up after this many games in one run refuse to archive.

A run now drains the whole backlog, so "keep going" has to have a floor under
it. Individual failures are excluded from later batches and tolerated -- one
corrupt game should not hold up twenty thousand -- but this many says the problem
is not the games.
-}
maxFailuresPerRun :: Int
maxFailuresPerRun = 50

-- | How often to look for restores a crashed replica left behind.
restorePollSeconds :: Int
restorePollSeconds = 60

{- | A restore that has not finished in this long is assumed to have died with
its replica and is picked up by whoever notices.
-}
restoreStaleMinutes :: Int
restoreStaleMinutes = 10

-- | The 'arkham_stats_refreshes' row the daily sweep claims and records itself in.
archiveSweepName :: Text
archiveSweepName = "game_archive_sweep"

{- | The oldest @updated_at@ a game may have and still be left alone.

Calendar months rather than @n * 30 days@: the setting says months, and a
retention promise of "six months" that quietly means 180 days is the kind of
difference someone eventually has to explain to a player.
-}
archiveCutoff :: Int -> UTCTime -> UTCTime
archiveCutoff months now =
  now {utctDay = addGregorianMonthsClip (negate $ fromIntegral months) (utctDay now)}

-- -------------------------------------------------------------------- misc ---

runDBIO :: App -> ReaderT SqlBackend IO a -> IO a
runDBIO app action = runSqlPool action (appConnPool app)

{- | Announce this transaction as the archiver, exempting it from the step
triggers for its duration. See the @arkham_game_archives@ migration: the
alternative is @DISABLE TRIGGER@, which is DDL on the hottest table in the
application and lifts the guard for every other session too.

@SET LOCAL@ rather than @SELECT set_config(..., true)@, which is the same thing
but as an expression: 'rawExecute' is postgresql-simple's @execute@, and that
refuses any statement returning columns, so the @SELECT@ form fails before it
sets anything.
-}
announceArchiving :: ReaderT SqlBackend IO ()
announceArchiving = rawExecute "SET LOCAL arkham.archiving = 'on'" []

{- | Log a line and flush it.

GHC gives stdout block buffering whenever it is not a terminal, which in a
container it never is. Everything this module has to say happens minutes or
hours apart -- what a sweep did, which game would not archive -- and a diagnostic
that sits in an 8KB buffer until something else fills it is no use to whoever is
watching the logs to decide whether this is safe to leave running.
-}
logArchive :: String -> IO ()
logArchive line = do
  putStrLn line
  hFlush stdout

gameParam :: ArkhamGameId -> PersistValue
gameParam = PersistText . toPathPiece

reason :: SomeException -> Text
reason = T.take 500 . T.pack . displayException

-- ----------------------------------------------------------------- reading ---

lookupArchive :: ArkhamGameId -> DB (Maybe ArkhamGameArchive)
lookupArchive gameId = fmap entityVal <$> P.getBy (UniqueGameArchive gameId)

{- | Games whose undo history is cold enough to offload, oldest first.

Three conditions, and the shape of the query matters more than it looks: this
runs against the table whose size is the problem, so it reads narrow indexed
columns and never touches @current_data@.

  * untouched since the cutoff,
  * has at least one step to collect, and
  * is not already archived or mid-restore.

A @restored@ game is a candidate again. It was asked for once, and it has since
gone quiet for another six months.
-}
archiveCandidates :: UTCTime -> Int -> [ArkhamGameId] -> DB [(ArkhamGameId, UTCTime)]
archiveCandidates cutoff n skip =
  map (bimap unValue unValue) <$> select do
    g <- from $ table @ArkhamGame
    where_ $ g.updatedAt <. val cutoff
    where_ $ exists do
      s <- from $ table @ArkhamStep
      where_ $ s.arkhamGameId ==. g.id
    {- Anything but fully restored is off limits, which is stricter than it looks
    and has to be.

    A game whose steps are in the bucket must obviously not be archived again.
    But neither must one whose restore FAILED: its old steps are still only in
    the object, its newer ones are in the database, and archiving those would
    write a second object holding the tail of the history and then delete the
    one holding the beginning. The re-archive path is only sound for a game
    whose rows are all present again. -}
    where_ $ notExists do
      a <- from $ table @ArkhamGameArchive
      where_ $ a.arkhamGameId ==. g.id
      where_ $ a.state !=. val Restored
    {- Games this run has already failed on.

    Without this the sweep cannot terminate. A failure writes no archive row, so
    the game stays a candidate -- and it is the OLDEST candidate, so the very next
    batch picks it up again, forever. The per-run ceiling used to hide that; a run
    that drains the whole backlog does not.

    Only when there are some: esqueleto renders an empty @notIn@ as a tautology,
    but an unnecessary clause in the query that walks the table this feature
    exists to shrink is worth not emitting. -}
    unless (null skip) $ where_ $ g.id `notIn` valList skip
    orderBy [asc g.updatedAt]
    limit (fromIntegral n)
    pure (g.id, g.updatedAt)

{- | How many games are waiting, for the panel.

Capped rather than exact. The count is a backlog gauge -- "lots" and "a few" are
the only two answers that change a decision -- and an uncapped @count(*)@ with
these predicates is a scan of the whole games table every time the page loads.
-}
archiveCandidateCount :: UTCTime -> Int -> DB Int
archiveCandidateCount cutoff cap = length <$> archiveCandidates cutoff cap []

-- --------------------------------------------------------------- archiving ---

data ArchiveOutcome = ArchiveOutcome
  { archiveOutcomeSteps :: Int
  , archiveOutcomeBytes :: Int
  , archiveOutcomeKey :: Text
  }

{- | Step rows per database page, and per @INSERT@ on the way back.

Both directions work in pages so that peak memory is a page rather than a game.
A long campaign's history is tens of megabytes of jsonb; six replicas sharing a
2Gi limit, with an autoscaler watching memory, is not somewhere to find out what
the biggest game on the site is.
-}
pageSize :: Int
pageSize = 500

-- | The suffix the prologue line ends with. See 'writeExport'.
stepsOpen :: BSL.ByteString
stepsOpen = "\"steps\":["

{- | Write the export to a handle, one step per line.

The document is exactly what the full-export endpoint serves -- so an archive can
be read, or fed straight to @POST \/games\/import@ -- but laid out so that
restoring it does not require parsing it. aeson never emits a newline inside a
value, so "one step per line" is a layout choice that costs nothing and makes
the way back a fold over lines. See 'stepLines'.

The layout is a contract between this function and that one:

> {"campaignPlayers":[...],"campaignData":{...,"steps":[
> {step}
> ,{step}
> ],"log":[],"multiplayerVariant":"Solo"}}

Returns how many steps were written and the highest step number among them. The
highest is what the delete is bounded by, so a step written after this read --
which for a game idle for six months should be impossible, but see the
@updated_at@ re-check below -- cannot be deleted without having been stored.
-}
writeExport :: App -> ArkhamGameId -> ArkhamGameRaw -> [Text] -> Handle -> IO (Int, Int)
writeExport app gameId raw players h = do
  BSL.hPut h
    $ "{\"campaignPlayers\":"
    <> Aeson.encode players
    <> ",\"campaignData\":{\"name\":"
    <> Aeson.encode raw.name
    <> ",\"currentData\":"
    <> Aeson.encode raw.currentData
    <> ",\"step\":"
    <> Aeson.encode raw.step
    <> ","
    <> stepsOpen
    <> "\n"
  (written, topStep) <- go (-1) 0 0
  BSL.hPut h
    $ "],\"log\":[],\"multiplayerVariant\":"
    <> Aeson.encode raw.multiplayerVariant
    <> "}}\n"
  pure (written, topStep)
 where
  go after written topStep = do
    rows <- runDBIO app $ stepPage gameId after pageSize
    case rows of
      [] -> pure (written, topStep)
      _ -> do
        for_ (zip [written ..] rows) \(i, row) -> do
          when (i > (0 :: Int)) $ BSL.hPut h ","
          BSL.hPut h $ Aeson.encode (stepValue gameId row) <> "\n"
        -- Rows come back ascending and every one is above @after@, so the
        -- highest in the page is both the next cursor and the new top step.
        let lastStep = foldl' max topStep [st | (_, Single st, _) <- rows]
        go lastStep (written + length rows) lastStep

-- | One page of a game's steps, oldest first.
stepPage
  :: ArkhamGameId
  -> Int
  -> Int
  -> ReaderT SqlBackend IO [(Single Aeson.Value, Single Int, Single Aeson.Value)]
stepPage gameId after n =
  rawSql
    "SELECT choice, step, action_diff FROM arkham_steps \
    \WHERE arkham_game_id = ? AND step > ? ORDER BY step ASC LIMIT ?"
    [gameParam gameId, PersistInt64 (fromIntegral after), PersistInt64 (fromIntegral n)]

{- | A step row as the export spells it.

The field names are the ones 'ArkhamStep' derives, which is why @actionDiff@ is
spelled the way it is rather than as its column name: this has to be the same
JSON the import endpoint reads.
-}
stepValue :: ArkhamGameId -> (Single Aeson.Value, Single Int, Single Aeson.Value) -> Aeson.Value
stepValue gameId (Single choice, Single step, Single actionDiff) =
  object
    [ "arkhamGameId" .= gameId
    , "choice" .= choice
    , "step" .= step
    , "actionDiff" .= actionDiff
    ]

objectKeyFor :: ArkhamGameId -> UTCTime -> ObjectKey
objectKeyFor gameId now =
  ObjectKey
    $ "archives/"
    <> toPathPiece gameId
    <> "/"
    <> T.pack (formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now)
    <> ".json.gz"

{- | A temp file that is removed however the action ends.

Not @withSystemTempFile@: @temporary@ is not a dependency, and this is the whole
of what it would be used for.
-}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile template action = do
  dir <- getTemporaryDirectory
  bracket (openBinaryTempFile dir template) cleanup (uncurry action)
 where
  -- Both are best-effort: the action closes the handle itself when it is done
  -- writing, and a leftover temp file must not mask the real result.
  cleanup (path, h) = do
    void $ tryAny $ hClose h
    void $ tryAny $ removeFile path

{- | Offload one game's undo history.

Write the export to a temp file, gzip it to another, upload it, __confirm the
upload__, and only then delete the rows -- with the delete and the archive row in
one transaction. A failure anywhere before the delete leaves an orphaned object
in the bucket, which costs pennies; a failure in the other order loses a
player's history.

Nothing is held in memory but a page of steps: both files are written and read
incrementally, and 'hashedFile' hashes and uploads in constant space.
-}
archiveGame :: App -> ArchiveConfig -> Env -> ArkhamGameId -> IO (Either Text ArchiveOutcome)
archiveGame app cfg env gameId = either (Left . reason) id <$> tryAny go
 where
  bucket = archiveConfigBucket cfg
  go = do
    now <- getCurrentTime
    mPayload <- runDBIO app do
      mRaw <- P.get (ArkhamGameRawKey gameId)
      case mRaw of
        Nothing -> pure Nothing
        Just raw -> do
          players <-
            map unValue <$> select do
              p <- from $ table @ArkhamPlayer
              where_ $ p.arkhamGameId ==. val gameId
              pure p.investigatorId
          -- A game that was restored and has gone quiet again already has a row
          -- and an object. The object it points at is superseded by this one.
          mPrevious <- lookupArchive gameId
          pure $ Just (raw, players, mPrevious)

    case mPayload of
      Nothing -> pure $ Left "game no longer exists"
      Just (raw, players, mPrevious) ->
        withTempFile "arkham-archive-.json" \rawPath rawHandle -> do
          (steps, topStep) <- writeExport app gameId raw players rawHandle
          hClose rawHandle
          if steps == 0
            then pure $ Left "game has no steps to archive"
            else withTempFile "arkham-archive-.json.gz" \gzPath gzHandle -> do
              hClose gzHandle
              {- The hash of what was written, to be checked against the hash of
              what comes back down. A separate lazy pass over the local file
              rather than something accumulated while writing: it costs one
              sequential read of a file that was just written, and it keeps
              'writeExport' to one job. -}
              digest <- SHA256.hashlazy <$> BSL.readFile rawPath
              -- Lazily: read, compress and write a chunk at a time.
              BSL.writeFile gzPath . GZip.compress =<< BSL.readFile rawPath
              size <- getFileSize gzPath

              let
                key = objectKeyFor gameId now
                ObjectKey keyText = key

              hashed <- hashedFile gzPath
              runResourceT do
                void
                  . send env
                  $ newPutObject bucket key (toBody hashed)
                  & (putObject_contentType ?~ "application/gzip")
                -- Prove it landed. Everything after this is a delete.
                void $ send env (newHeadObject bucket key)

              -- Read the object back before anything is deleted. See 'verifyArchive'.
              verified <- verifyArchive env (coerce bucket) keyText steps digest
              case verified of
                Left err -> do
                  -- Nothing has been deleted yet, so the object is of no use to
                  -- anyone: take it back out rather than leave a file a future
                  -- restore would find and refuse.
                  void $ tryAny $ runResourceT $ send env $ newDeleteObject bucket key
                  pure $ Left err
                Right () -> do
                  committed <- runDBIO app do
                    announceArchiving
                    -- Serialise against play. 'atomicallyWithGame' -- the path every
                    -- action goes through -- takes this same row lock, so holding it
                    -- and finding @updated_at@ unchanged means nothing has been
                    -- played since the export was read. A game idle for six months
                    -- being played in that window is far-fetched, but the failure it
                    -- would cause is not: deleting a step that was never stored
                    -- leaves a hole the insert trigger refuses to fill, and the game
                    -- can never take another action.
                    current <- select do
                      g <- from $ table @ArkhamGame
                      where_ $ g.id ==. val gameId
                      locking forUpdate
                      pure g.updatedAt
                    if map unValue current /= [raw.updatedAt]
                      then pure False
                      else do
                        rawExecute
                          "DELETE FROM arkham_steps WHERE arkham_game_id = ? AND step <= ?"
                          [gameParam gameId, PersistInt64 (fromIntegral topStep)]
                        void
                          $ P.upsertBy
                            (UniqueGameArchive gameId)
                            ( ArkhamGameArchive
                                gameId
                                Archived
                                (coerce bucket)
                                keyText
                                steps
                                topStep
                                (fromIntegral size)
                                now
                                raw.updatedAt
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                            )
                            [ ArkhamGameArchiveState P.=. Archived
                            , ArkhamGameArchiveBucket P.=. coerce bucket
                            , ArkhamGameArchiveObjectKey P.=. keyText
                            , ArkhamGameArchiveSteps P.=. steps
                            , ArkhamGameArchiveTopStep P.=. topStep
                            , ArkhamGameArchiveBytes P.=. fromIntegral size
                            , ArkhamGameArchiveArchivedAt P.=. now
                            , ArkhamGameArchiveGameUpdatedAt P.=. raw.updatedAt
                            , ArkhamGameArchiveRequestedAt P.=. Nothing
                            , ArkhamGameArchiveRequestedBy P.=. Nothing
                            , ArkhamGameArchiveRestoredAt P.=. Nothing
                            , ArkhamGameArchiveErrorMessage P.=. Nothing
                            ]
                        pure True

                  if not committed
                    then pure $ Left "game was played while it was being archived"
                    else do
                      -- Only once the replacement is recorded, and best-effort: an
                      -- object that outlives its row costs storage and makes the
                      -- reclaimed-bytes figure a lie, but failing the archive over
                      -- it would be worse.
                      for_ mPrevious \previous ->
                        when (previous.objectKey /= keyText)
                          $ void
                          $ tryAny
                          $ runResourceT
                          $ send env
                          $ newDeleteObject (BucketName previous.bucket) (ObjectKey previous.objectKey)

                      pure
                        $ Right
                          ArchiveOutcome
                            { archiveOutcomeSteps = steps
                            , archiveOutcomeBytes = fromIntegral size
                            , archiveOutcomeKey = keyText
                            }

{- | Archive one game, chosen by hand.

The sweep's entry point for a single game, with its own 'Env' because there is
no run to share one with. This is deliberately not subject to the retention
cutoff: the whole point of it is that somebody has decided about this game.
-}
archiveOne :: App -> ArchiveConfig -> ArkhamGameId -> IO (Either Text ArchiveOutcome)
archiveOne app cfg gameId = do
  result <-
    either (Left . reason) id <$> tryAny do
      env <- newEnv discover
      archiveGame app cfg env gameId
  {- Logged as well as returned. The caller is a synchronous request, and for a
  large game that request can be cut off by a proxy long before the work ends;
  without this there would be no record of how it went. -}
  logArchive $ case result of
    Right outcome ->
      "archive: archived game "
        <> T.unpack (toPathPiece gameId)
        <> " by hand: "
        <> show (archiveOutcomeSteps outcome)
        <> " step(s), "
        <> show (archiveOutcomeBytes outcome)
        <> " byte(s) stored"
    Left err ->
      "archive: hand-archiving game "
        <> T.unpack (toPathPiece gameId)
        <> " FAILED: "
        <> T.unpack err
  pure result

-- --------------------------------------------------------------- restoring ---

{- | Take the game from 'Archived' to 'Restoring', if it is there to be taken.

'Nothing' means somebody else got there first, which is the case this exists for:
two players pressing undo must not start two downloads. The conditional update is
what makes that safe -- the second transaction blocks on the row, re-evaluates its
@WHERE@, and finds the state already moved on.
-}
claimForRestore :: UTCTime -> Maybe UserId -> ArkhamGameId -> DB (Maybe ArkhamGameArchive)
claimForRestore now mUserId gameId = do
  n <- updateCount \a -> do
    set
      a
      [ ArkhamGameArchiveState =. val Restoring
      , ArkhamGameArchiveRequestedAt =. val (Just now)
      , ArkhamGameArchiveRequestedBy =. val mUserId
      , ArkhamGameArchiveErrorMessage =. val Nothing
      ]
    where_ $ a.arkhamGameId ==. val gameId
    -- 'Failed' as well as 'Archived': the object is still in the bucket, and a
    -- person asking again is the only sensible retry for a restore that broke.
    where_ $ a.state `in_` valList [Archived, Failed]
  if n == 0 then pure Nothing else lookupArchive gameId

{- | The step objects out of an archive, without parsing one.

'writeExport' puts each step on its own line, so this drops the prologue line,
stops at the line that closes the array, and strips the separating comma. The
alternative is decoding the whole document, and for a long campaign that is
hundreds of megabytes of 'Value' in order to put a few thousand rows back.

It is also why a six-month-old archive can be restored at all: an engine that
has moved on may no longer be able to decode the messages inside these objects,
and nothing here asks it to. See the module header.
-}
stepLines :: BSL.ByteString -> Either Text [BSL.ByteString]
stepLines raw = case BSL8.lines raw of
  [] -> Left "archive is empty"
  prologue : rest
    | not (stepsOpen `BSL.isSuffixOf` prologue) ->
        Left "archive is not in the line-per-step form a restore reads"
    | otherwise -> Right $ map dropComma $ takeWhile (not . closesArray) rest
 where
  closesArray l = "]" `BSL.isPrefixOf` l
  dropComma l = if "," `BSL.isPrefixOf` l then BSL.drop 1 l else l

{- | Download an archive to a temp file and hand over its path.

Streamed to disk, never held in memory: the largest game measured locally is
522MB of step JSON. The caller reads it back with 'archiveBody', once per pass
it needs -- a fresh lazy read each time, rather than one value used twice, which
would pin the whole thing in the heap between the two uses.
-}
withArchiveFile :: Env -> Text -> Text -> (FilePath -> IO a) -> IO a
withArchiveFile env bucket key action =
  withTempFile "arkham-archive-read-.json.gz" \path h -> do
    hClose h
    runResourceT do
      resp <- send env $ newGetObject (BucketName bucket) (ObjectKey key)
      sinkBody (view getObjectResponse_body resp) (sinkFile path)
    action path

{- | One lazy pass over a downloaded archive, decompressed.

Tolerant of an uncompressed object: everything written since this was added is
gzipped, and a plain one would be something put in the bucket by hand.
-}
archiveBody :: FilePath -> IO BSL.ByteString
archiveBody path = do
  body <- BSL.readFile path
  pure $ if isGzipped body then GZip.decompress body else body
 where
  isGzipped bs = BSL.take 2 bs == BSL.fromStrict (BS.pack [0x1f, 0x8b])

{- | Read the uploaded object back and prove it is the file that was written.

Belt and braces, and deliberately so: the next thing 'archiveGame' does is
delete the only other copy. Three separate things are checked, because they fail
differently.

  * The SHA-256 of the decompressed bytes, against the hash of the file that was
    written. This is the one that says no byte changed on the way up and nothing
    was truncated -- a count or a size can match while the contents do not.
  * That 'stepLines' -- the reader a restore will use, months from now and
    possibly several engine versions later -- can read it at all.
  * That it finds the number of steps that were written.

Two passes over the download rather than one, each its own lazy read, because
holding the body for a second use would pin hundreds of megabytes.

The cost is one extra download per archived game. Against finding out at restore
time that the writer and the reader had drifted apart, it is nothing.
-}
verifyArchive :: Env -> Text -> Text -> Int -> ByteString -> IO (Either Text ())
verifyArchive env bucket key expectedSteps expectedDigest =
  either (Left . reason) id <$> tryAny do
    withArchiveFile env bucket key \path -> do
      digest <- SHA256.hashlazy <$> archiveBody path
      if digest /= expectedDigest
        then
          pure
            $ Left
            $ "uploaded archive does not match what was written (sha256 "
            <> hexOf digest
            <> ", expected "
            <> hexOf expectedDigest
            <> ")"
        else
          archiveBody path >>= \body -> case stepLines body of
            Left err -> pure $ Left $ "uploaded archive does not read back: " <> err
            Right ls -> do
              -- 'length' walks the lazy list without holding on to it.
              let found = length ls
              pure
                $ if found == expectedSteps
                  then Right ()
                  else
                    Left
                      $ "uploaded archive has "
                      <> tshow found
                      <> " steps, expected "
                      <> tshow expectedSteps

hexOf :: ByteString -> Text
hexOf = decodeUtf8 . B16.encode

{- | Download the archive and put the steps back.

Steps go back in through @jsonb_to_recordset@, a page at a time, so nothing
decodes them and nothing holds more than a page. One transaction, so a restore
either happened or did not.

@ON CONFLICT DO NOTHING@ because the game may have been played since it was
archived. Those steps are above the archive's top step and are the ones to keep;
the clause makes the restore idempotent as well, so a retry after a half-finished
attempt is harmless.
-}
restoreGame :: App -> ArkhamGameArchive -> IO (Either Text Int)
restoreGame app archive = either (Left . reason) id <$> tryAny go
 where
  go = do
    env <- newEnv discover
    withArchiveFile env archive.bucket archive.objectKey \path -> do
      archiveBody path >>= \body -> case stepLines body of
        Left err -> pure $ Left $ "could not read archive: " <> err
        Right steps -> do
          now <- getCurrentTime
          restored <- runDBIO app do
            announceArchiving
            written <- for (chunksOf pageSize steps) \chunk -> do
              rawExecute
                "INSERT INTO arkham_steps (arkham_game_id, choice, step, action_diff) \
                \SELECT ?, s.choice, s.step, s.\"actionDiff\" \
                \FROM jsonb_to_recordset(?::jsonb) \
                \  AS s(choice jsonb, step int, \"actionDiff\" jsonb) \
                \ON CONFLICT (arkham_game_id, step) DO NOTHING"
                [ gameParam archive.arkhamGameId
                , PersistText $ decodeUtf8 $ BSL.toStrict $ "[" <> BSL.intercalate "," chunk <> "]"
                ]
              pure $ length chunk
            void $ updateCount \a -> do
              set
                a
                [ ArkhamGameArchiveState =. val Restored
                , ArkhamGameArchiveRestoredAt =. val (Just now)
                , ArkhamGameArchiveErrorMessage =. val Nothing
                ]
              where_ $ a.arkhamGameId ==. val archive.arkhamGameId
            pure (sum written)
          pure $ Right restored

{- | Run a claimed restore, and tell the table how it went either way.

Forked rather than awaited: a restore is a download plus thousands of inserts,
and the request that asked for it is a button press. The result reaches the
player over the game's websocket, which is also how it reaches them when the
replica that served the request is not the one holding their socket.
-}
startRestore :: App -> ArkhamGameArchive -> IO ()
startRestore app archive = void $ forkIO $ void $ tryAny do
  result <- restoreGame app archive
  case result of
    Right n -> do
      publishFromApp app archive.arkhamGameId GameUnarchived
      logArchive
        $ "archive: restored "
        <> show n
        <> " steps for game "
        <> T.unpack (toPathPiece archive.arkhamGameId)
    Left err -> do
      now <- getCurrentTime
      runDBIO app $ void $ updateCount \a -> do
        set
          a
          [ ArkhamGameArchiveState =. val Failed
          , ArkhamGameArchiveErrorMessage =. val (Just err)
          , ArkhamGameArchiveRestoredAt =. val (Just now)
          ]
        where_ $ a.arkhamGameId ==. val archive.arkhamGameId
      publishFromApp app archive.arkhamGameId $ GameUnarchiveFailed err
      logArchive
        $ "archive: restore FAILED for game "
        <> T.unpack (toPathPiece archive.arkhamGameId)
        <> ": "
        <> T.unpack err

{- | Pick up restores a replica died in the middle of.

Claimed one at a time by bumping @requested_at@: whoever wins the row has it, and
the loser's @WHERE@ no longer matches. Without this a game whose restore was
interrupted would sit in 'Restoring' forever, refusing undo and refusing to
retry.
-}
restoreWorker :: App -> IO ()
restoreWorker app = case archiveConfig (appSettings app) of
  Nothing -> pure ()
  Just _ -> forever do
    threadDelay (restorePollSeconds * 1000000)
    void $ tryAny do
      now <- getCurrentTime
      let staleBefore = addUTCTime (negate $ fromIntegral restoreStaleMinutes * 60) now
      stale <- runDBIO app $ claimStale now staleBefore
      for_ stale \archive -> do
        logArchive
          $ "archive: picking up abandoned restore for game "
          <> T.unpack (toPathPiece archive.arkhamGameId)
        startRestore app archive
 where
  claimStale now staleBefore = do
    candidates <- select do
      a <- from $ table @ArkhamGameArchive
      where_ $ a.state ==. val Restoring
      where_ $ a.requestedAt <. just (val staleBefore)
      orderBy [asc a.requestedAt]
      limit 5
      pure a
    catMaybes <$> for candidates \(Entity _ archive) -> do
      n <- updateCount \a -> do
        set a [ArkhamGameArchiveRequestedAt =. val (Just now)]
        where_ $ a.arkhamGameId ==. val archive.arkhamGameId
        where_ $ a.state ==. val Restoring
        where_ $ a.requestedAt <. just (val staleBefore)
      pure $ if n == 1 then Just archive else Nothing

-- ------------------------------------------------------------------- sweep ---

data ArchiveSweep = ArchiveSweep
  { archiveSweepArchived :: Int
  , archiveSweepFailed :: Int
  , archiveSweepBytes :: Int
  , archiveSweepSteps :: Int
  }

{- | Claim today's run, across every replica.

One statement, and the row that records when the sweep last ran is also the lock
that stops a second replica starting another. @ON CONFLICT ... WHERE@ makes the
claim and the guard the same decision, so there is no window between checking and
taking.
-}
claimSweep :: Int -> DB Bool
claimSweep intervalHours = do
  n <-
    rawExecuteCount
      "INSERT INTO arkham_stats_refreshes (name, refreshed_at, duration_ms) \
      \VALUES (?, now(), 0) \
      \ON CONFLICT (name) DO UPDATE SET refreshed_at = now(), duration_ms = 0 \
      \WHERE arkham_stats_refreshes.refreshed_at < now() - make_interval(hours => ?)"
      [PersistText archiveSweepName, PersistInt64 (fromIntegral intervalHours)]
  pure $ n > 0

{- | Stamp the run's cost on the row claimed above, so the panel can say what the
sweep costs as well as when it last happened.
-}
recordSweep :: Int -> DB ()
recordSweep durationMs =
  rawExecute
    "UPDATE arkham_stats_refreshes SET duration_ms = ? WHERE name = ?"
    [PersistInt64 (fromIntegral durationMs), PersistText archiveSweepName]

{- | Archive in batches until the backlog is empty.

A run drains everything it can reach. The batch size and the pause between
batches are the only throttle: the point is that the deletes land as a trickle
against the busiest table in the application rather than as one burst, not that
the run stops early. At the default 10 games every 20 seconds a backlog of
twenty thousand games is about eleven hours, which is one night.

Three ways a run ends. The backlog runs out, which is the normal one. A whole
batch fails, which means the bucket is wrong or the credentials are refused and
the useful response is one error rather than twenty thousand. Or maintenance
mode comes on, because a run is thousands of deletes against the table whose
lock someone has just taken the site down to get.

Individual failures are tolerated and excluded from later batches: one corrupt
game must not hold up the rest, and re-selecting it would mean never finishing.
See 'archiveCandidates'.
-}
runArchiveSweep :: App -> ArchiveConfig -> IO ArchiveSweep
runArchiveSweep app cfg = do
  {- One 'Env' for the whole run, not one per game.

  'newEnv' builds an HTTP manager and resolves credentials; a run that drains
  twenty thousand games would otherwise do that twenty thousand times. It also
  means a credentials problem fails here, at the start, rather than once per
  game. -}
  env <- newEnv discover
  loop env (ArchiveSweep 0 0 0 0) []
 where
  loop env acc skip
    | archiveSweepFailed acc >= maxFailuresPerRun = do
        logArchive
          $ "archive: "
          <> show (archiveSweepFailed acc)
          <> " failures, ending this run"
        pure acc
    | otherwise =
        underMaintenance app >>= \case
          True -> do
            logArchive "archive: maintenance mode is on, ending this run"
            pure acc
          False -> do
            now <- getCurrentTime
            let cutoff = archiveCutoff (archiveConfigAfterMonths cfg) now
            candidates <-
              runDBIO app $ archiveCandidates cutoff (archiveConfigBatchSize cfg) skip
            if null candidates
              then pure acc
              else do
                (acc', failed) <- foldM (step env) (acc, []) (map fst candidates)
                if archiveSweepArchived acc' == archiveSweepArchived acc
                  then do
                    logArchive "archive: whole batch failed, ending this run"
                    pure acc'
                  else do
                    threadDelay (archiveConfigPauseSeconds cfg * 1000000)
                    loop env acc' (failed <> skip)

  step env (acc, failed) gameId =
    archiveGame app cfg env gameId >>= \case
      Right outcome ->
        pure
          ( acc
              { archiveSweepArchived = archiveSweepArchived acc + 1
              , archiveSweepBytes = archiveSweepBytes acc + archiveOutcomeBytes outcome
              , archiveSweepSteps = archiveSweepSteps acc + archiveOutcomeSteps outcome
              }
          , failed
          )
      Left err -> do
        logArchive
          $ "archive: could not archive game "
          <> T.unpack (toPathPiece gameId)
          <> ": "
          <> T.unpack err
        pure (acc {archiveSweepFailed = archiveSweepFailed acc + 1}, gameId : failed)

{- | Whether the app is deliberately refusing traffic.

The sweep asks before every batch. Maintenance mode is how a lock is taken on
this database by hand (see "Api.Arkham.Maintenance"), and a background job
deleting thousands of rows through it is exactly what that window exists to
prevent -- it is also invisible to the middleware, which only refuses HTTP.
-}
underMaintenance :: App -> IO Bool
underMaintenance app = isJust <$> readTVarIO (appMaintenance app)

{- | The daily job, which also runs a minute after this replica starts.

__Does nothing unless @ARCHIVE_SWEEP@ says so__, separately from the bucket
being configured. A bucket makes archiving possible; this is what lets a job
decide for itself which games to do it to. The intended order is to archive a
game you picked, restore it, and only then hand the backlog over.

A thread per replica that tries the claim at boot and then hourly, doing nothing
unless it wins, rather than a cron entry or a Kubernetes @CronJob@: the work
needs the connection pool, the settings and the websocket broker this process
already has, and a second deployment artefact to run one query a day is a lot of
moving parts to keep in step with the application it is part of.

__Boot is the kick-off.__ Turning the feature on is a deploy, and waiting an hour
for the first sweep -- or having to go and press a button -- is a worse answer than
starting a minute after the pod is up. It is safe because the claim is the same
one the hourly tick uses: a rolling restart of six replicas produces at most one
run, and only if today's has not already happened.
-}

{- | Run a sweep and stamp its cost on the row that was claimed for it.

Shared by the daily thread and the admin button so that a hand-started run is
the same run, recorded the same way, and counts against the same claim.
-}
runAndRecordSweep :: App -> ArchiveConfig -> IO ArchiveSweep
runAndRecordSweep app cfg = do
  startedAt <- getCurrentTime
  {- Record the run even if it threw.

  The claim is already taken, so a run that dies without stamping its cost
  leaves a row saying a sweep started today and nothing saying what happened --
  which is indistinguishable, from the panel, from a sweep still going. The one
  realistic thrower is 'newEnv', on credentials. -}
  sweep <-
    tryAny (runArchiveSweep app cfg) >>= \case
      Right sweep -> pure sweep
      Left err -> do
        logArchive $ "archive: sweep failed: " <> T.unpack (reason err)
        pure (ArchiveSweep 0 1 0 0)
  finishedAt <- getCurrentTime
  let elapsedMs = round $ 1000 * diffUTCTime finishedAt startedAt
  runDBIO app $ recordSweep elapsedMs
  logArchive
    $ "archive: sweep archived "
    <> show (archiveSweepArchived sweep)
    <> " game(s), "
    <> show (archiveSweepSteps sweep)
    <> " step(s), "
    <> show (archiveSweepBytes sweep)
    <> " byte(s) stored, "
    <> show (archiveSweepFailed sweep)
    <> " failure(s) in "
    <> show elapsedMs
    <> "ms"
  pure sweep

archiveSweeper :: App -> IO ()
archiveSweeper app = case archiveConfig (appSettings app) of
  _ | not (appArchiveSweepEnabled (appSettings app)) -> pure ()
  Nothing -> pure ()
  Just cfg -> do
    threadDelay (bootDelaySeconds * 1000000)
    forever do
      void $ tryAny do
        -- Not during a maintenance window. 'runArchiveSweep' checks again before
        -- every batch, but there is no reason to claim the day's run for one that
        -- is about to stop.
        quiet <- underMaintenance app
        unless quiet do
          claimed <- runDBIO app $ claimSweep sweepIntervalHours
          when claimed $ void $ runAndRecordSweep app cfg
      threadDelay (sweepPollSeconds * 1000000)

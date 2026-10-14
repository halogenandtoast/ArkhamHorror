{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Taking a game's log away with you.

The debug exports next door deliberately carry no log at all
(@Api.Arkham.Export@ sets @agedLog = []@), because what they are for is feeding
a game back into the engine, and the log is derived from the message stream
rather than part of the state. This is the other thing someone wants from a
finished campaign: the story, as a file.

Two endpoints, because the scope picker needs to know what there is to pick
before anything is exported:

* 'getApiV1ArkhamGameLogSectionsR' says which scenarios the log covers.
* 'getApiV1ArkhamGameLogExportR' hands over a step range of it.

The readable transcript is rendered on the __client__, from @format=json@.
Every sentence in a structured entry is an i18n key, and the locales live in
the frontend -- a transcript rendered here would read @$log.scenarioBegins@.
@format=trace@ is that lossy rendering anyway, for the times the keys are what
you want to see.
-}
module Api.Handler.Arkham.Game.LogExport (
  getApiV1ArkhamGameLogSectionsR,
  getApiV1ArkhamGameLogExportR,
) where

import Api.Arkham.Helpers
import Arkham.Card.CardCode
import Arkham.Log.Entry
import Conduit
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.Esqueleto.Experimental hiding (update, (==.))
import Database.Esqueleto.Experimental qualified as E
import Database.Persist qualified as Persist
import Entity.Arkham.LogEntry
import Import hiding (delete, exists, on, (==.))
import Json

{- | One scenario's worth of log, as a range of game steps.

@untilStep@ is exclusive and is the next scenario's @fromStep@, so an interlude
played after a resolution falls inside the scenario it followed rather than
into a gap. The last section has no bound: it runs to the end of the log.

A game whose log predates the structured overhaul has no sections at all --
there is no @log.scenarioBegins@ row to find -- and the client offers only the
whole log for it. That is honest; the alternative is guessing boundaries out of
@arkham_steps@, which is the 94GB table.
-}
data LogSection = LogSection
  { index :: Int
  , name :: Maybe Text
  , cardCode :: Maybe CardCode
  , fromStep :: Int
  , untilStep :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | Every ref an entry's sentence mentions, variables and lists included.

A 'LogI18n' holds its variables as parts, which is where a scenario's name
actually lives: the body is one key with @scenario@ bound to the ref.
-}
logPartRefs :: LogPart -> [LogRef]
logPartRefs = \case
  LogRefPart r -> [r]
  LogI18n _ vars -> concatMap logPartRefs (Map.elems vars)
  LogList ps -> concatMap logPartRefs ps
  _ -> []

scenarioRefOf :: LogEntry -> Maybe LogRef
scenarioRefOf e =
  listToMaybe [r | r <- concatMap logPartRefs e.logEntryBody, r.logRefKind == RefScenario]

{- | The rows that open a scenario.

Matched on @body@ rather than on the JSON payload: a structured row's @body@ is
'logEntryToText' of the entry, so the narrator's banner always starts with the
key itself (@$log.scenarioBegins(scenario=...)@). That keeps this a cheap
prefix match on one column instead of a jsonb traversal, and the game filter
carries the index.
-}
scenarioBanners :: ArkhamGameId -> DB [(Int, Maybe Aeson.Value)]
scenarioBanners gameId = do
  rows <- select do
    e <- from $ table @ArkhamLogEntry
    where_ $ e.arkhamGameId E.==. val gameId
    -- Parenthesised: `like` is infixr 2, as Admin/Users.hs notes.
    where_ $ e.body `like` (val "$log.scenarioBegins" ++. (%))
    orderBy [asc e.step, asc e.id]
    pure (e.step, e.payload)
  pure [(step, payload) | (Value step, Value payload) <- rows]

toSections :: [(Int, Maybe Aeson.Value)] -> [LogSection]
toSections banners = zipWith section [1 ..] (zip banners bounds)
 where
  -- Each section runs to where the next one starts; the last one is open-ended.
  bounds = map (Just . fst) (drop 1 banners) <> [Nothing]
  section i ((step, payload), untilStep) =
    let mRef = do
          v <- payload
          entry <- case Aeson.fromJSON v of
            Aeson.Success entry -> Just entry
            Aeson.Error _ -> Nothing
          scenarioRefOf entry
     in LogSection
          { index = i
          , -- No name rather than an English one: the client localizes a
            -- scenario's name from its card code anyway, and has the wording
            -- for a section it cannot name. A payload that will not decode
            -- still gets a row -- the reader knows a scenario started here, and
            -- the range is right either way.
            name = (.logRefName) <$> mRef
          , cardCode = mRef >>= (.logRefCardCode)
          , fromStep = step
          , untilStep = untilStep
          }

{- | Which scenarios this game's log covers, for the export dialog's scope
picker. Membership is checked the way the log scrollback is.
-}
getApiV1ArkhamGameLogSectionsR :: ArkhamGameId -> Handler [LogSection]
getApiV1ArkhamGameLogSectionsR gameId = do
  userId <- getRequestUserId
  runDB do
    void $ getBy404 (UniquePlayer userId gameId)
    toSections <$> scenarioBanners gameId

{- | A step range of the log.

@fromStep@ (inclusive) and @untilStep@ (exclusive) come from a 'LogSection';
both absent is the whole game. @format@ is @json@ (the rows, which is what the
client renders a transcript from) or @trace@ (the server's own lossy text).
-}
getApiV1ArkhamGameLogExportR :: ArkhamGameId -> Handler TypedContent
getApiV1ArkhamGameLogExportR gameId = do
  userId <- getRequestUserId
  ge <- runDB do
    void $ getBy404 (UniquePlayer userId gameId)
    get404 gameId
  mFrom <- parseStep "fromStep"
  mUntil <- parseStep "untilStep"
  mScenario <- lookupGetParam "scenario"
  format <- fromMaybe "json" <$> lookupGetParam "format"
  let base = "arkham-log-" <> toPathPiece gameId
  case format of
    "trace" -> do
      addHeader "Content-Disposition" $ "attachment; filename=" <> base <> ".txt"
      respondSource "text/plain; charset=utf-8"
        $ traceSource ge.name mScenario (logRowSource gameId mFrom mUntil)
        .| sendChunks
    _ -> do
      addHeader "Content-Disposition" $ "attachment; filename=" <> base <> ".json"
      respondSource "application/json"
        $ jsonSource ge.name mScenario mFrom mUntil (logRowSource gameId mFrom mUntil)
        .| sendChunks
 where
  parseStep p = (>>= (readMaybe . T.unpack)) <$> lookupGetParam p
  sendChunks = awaitForever \chunk -> sendChunkBS chunk >> sendFlush

{- | The rows of a range, oldest first.

Streamed, and ordered by @(step, id)@ like every other log read. A campaign's
log is thousands of rows with a JSON payload on each; holding one scenario in
memory would be fine, but holding a whole campaign to build one response body
is the kind of thing that only shows up in production.
-}
logRowSource
  :: ArkhamGameId -> Maybe Int -> Maybe Int -> ConduitT () ArkhamLogEntry Handler ()
logRowSource gameId mFrom mUntil = do
  let filters =
        [ArkhamLogEntryArkhamGameId Persist.==. gameId]
          <> maybe [] (\s -> [ArkhamLogEntryStep Persist.>=. s]) mFrom
          <> maybe [] (\s -> [ArkhamLogEntryStep Persist.<. s]) mUntil
  acquire <-
    lift $ runDB $ Persist.selectSourceRes filters [Asc ArkhamLogEntryStep, Asc ArkhamLogEntryId]
  (_, rows) <- allocateAcquire acquire
  rows .| mapC entityVal

{- | The rows as JSON, in an envelope saying what was exported.

The rows themselves are 'LogRow's, exactly as @\/log\/before@ serves them, so
the client decodes them with the decoder it already has. The envelope is for
the person who keeps the file: a bare array says nothing about which game or
which scenario it came from.
-}
jsonSource
  :: Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> ConduitT () ArkhamLogEntry Handler ()
  -> ConduitT () BS.ByteString Handler ()
jsonSource gameName mScenario mFrom mUntil rows = do
  yieldBS "{\"gameName\":"
  yieldLBS $ encode gameName
  yieldBS ",\"scenario\":"
  yieldLBS $ encode mScenario
  yieldBS ",\"fromStep\":"
  yieldLBS $ encode mFrom
  yieldBS ",\"untilStep\":"
  yieldLBS $ encode mUntil
  yieldBS ",\"rows\":["
  isFirstRef <- liftIO $ newIORef True
  rows .| awaitForever \row -> do
    -- Not `first`: Relude re-exports Data.Bifunctor's, and -Wname-shadowing is
    -- an error here.
    isFirst <- liftIO $ readIORef isFirstRef
    unless isFirst $ yieldBS ","
    liftIO $ writeIORef isFirstRef False
    yieldLBS $ encode $ toLogRow row
  yieldBS "]}"

{- | The server's own text rendering: 'logEntryToLines', which is what
@arkham-replay --trace@ prints.

Lossy on purpose. An i18n key prints as the key and a legacy row prints its raw
brace DSL, because there is no locale here -- see the module header.
-}
traceSource
  :: Text
  -> Maybe Text
  -> ConduitT () ArkhamLogEntry Handler ()
  -> ConduitT () BS.ByteString Handler ()
traceSource gameName mScenario rows = do
  yieldLine $ "# " <> gameName
  for_ mScenario \scenario -> yieldLine $ "# " <> scenario
  yieldLine ""
  rows .| awaitForever \row -> case toLogRow row of
    LogRowStructured entry -> traverse_ yieldLine (logEntryToLines entry)
    LogRowLegacy body _ -> yieldLine body

yieldBS :: Monad m => BS.ByteString -> ConduitT i BS.ByteString m ()
yieldBS = yield

yieldLBS :: Monad m => BSL.ByteString -> ConduitT i BS.ByteString m ()
yieldLBS = traverse_ yield . BSL.toChunks

yieldLine :: Monad m => Text -> ConduitT i BS.ByteString m ()
yieldLine l = yieldBS $ encodeUtf8 l <> "\n"

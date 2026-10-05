{-# LANGUAGE TemplateHaskell #-}

{- | The wire format for a game-log entry.

This module is deliberately a leaf: it imports only the id types, because
"Arkham.Classes.GameLogger" imports it and almost everything imports that.
That is why names, chaos-token faces and skill icons are carried as 'Text'
rather than as 'Arkham.Name.Name', 'Arkham.ChaosToken.Types.ChaosTokenFace'
and 'Arkham.SkillType.SkillType' -- all three of those modules import
'GameLogger' for their 'ToGameLoggerFormat' instance, so using them here would
cycle. The typed constructors live in "Arkham.Log", which sits above all of it
and is what callers actually use.

See @docs/game-log/@ for the design and the journal.
-}
module Arkham.Log.Entry where

import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.Id
import Arkham.Json
import Arkham.Prelude
import Data.Aeson.TH
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

{- | What sort of thing a 'LogRef' points at, which is how the client decides
what to draw and what art to hover.
-}
data LogRefKind
  = RefCard
  | RefInvestigator
  | RefEnemy
  | RefLocation
  | RefAsset
  | RefEvent
  | RefSkill
  | RefTreachery
  | RefAct
  | RefAgenda
  | RefStory
  | RefScenario
  | RefAbility
  deriving stock (Show, Eq, Ord, Data)

{- | A pointer to something in the game, rendered as a chip the reader can hover
for the card.

A record rather than one constructor per kind, on purpose. The brace DSL this
replaces grew a separate shape per kind and then a second /arity/ for
locations, which the client had to try in order (see @GameMessage.vue@); every
new kind meant another regex branch that ran on every fragment of every render.
Here the client has one decoder keyed on 'logRefKind', and a new kind changes
nothing on the wire. Type safety is restored at the other end, by the smart
constructors in "Arkham.Log", which is where the author needs it.
-}
data LogRef = LogRef
  { logRefKind :: LogRefKind
  , logRefName :: Text
  -- ^ Already-displayable text. Localization of card names is the client's job.
  , logRefCardCode :: Maybe CardCode
  , logRefCardId :: Maybe CardId
  {- ^ The specific copy, when there is one. Two Magnifying Glasses are
  different chips.
  -}
  , logRefEntityId :: Maybe Text
  {- ^ The in-play entity's id, as text: an 'InvestigatorId', 'EnemyId',
  'LocationId' and so on. Text because this is a wire format and the client
  only ever looks it up; the constructors in "Arkham.Log" take the real type.
  -}
  , logRefFaceDown :: Bool
  {- ^ Draw the back. An unrevealed location is not the same chip as a revealed
  one.
  -}
  }
  deriving stock (Show, Eq, Ord, Data)

-- | A 'LogRef' with nothing filled in, for the smart constructors to build on.
logRef :: LogRefKind -> Text -> LogRef
logRef kind name =
  LogRef
    { logRefKind = kind
    , logRefName = name
    , logRefCardCode = Nothing
    , logRefCardId = Nothing
    , logRefEntityId = Nothing
    , logRefFaceDown = False
    }

{- | One piece of an entry's sentence.

'LogI18n' is the important one: its variables are themselves 'LogPart's, so a
localized template can still carry card chips. Today an author has to choose
between @ikey'@ (localized, plain text) and @format@ (rich, hardcoded
English), and because they compose only by @<>@ most call sites gave up and
wrote English. That choice is what this removes.
-}
data LogPart
  = LogText Text
  | {- | An i18n key (an @Arkham.I18n.Scope@, which is 'Text') plus its named
    variables, each of which may itself be rich.
    -}
    LogI18n Text (Map Text LogPart)
  | LogRefPart LogRef
  | LogNumber Int
  | -- | A signed modifier, drawn as @+2@ / @-1@ rather than as a bare number.
    LogDelta Int
  | -- | A chaos token face, by its wire name (@"PlusOne"@, @"Cultist"@).
    LogToken Text
  | -- | A skill icon, by its wire name (@"willpower"@).
    LogIcon Text
  | {- | A campaign-log key, sent as its own JSON rather than as a resolved
    string.

    The client already owns this mapping: @formatKey@ in @types/Log.ts@ turns a
    key into its i18n path, handling the campaign prefix, the @.key.@ segment,
    nested sections, homebrew scopes and apostrophes, and the campaign-log
    screen renders every key through it. Reimplementing that in Haskell would be
    a second copy to drift -- the first attempt guessed
    @campaignLog.\<ShownKey>@ and rendered the key itself.
    -}
    LogCampaignKey Value
  | {- | Several parts joined by the locale's list rule, so "a, b, and c" is not
    built in Haskell.
    -}
    LogList [LogPart]
  deriving stock (Show, Eq, Ord, Data)

{- | What kind of thing happened, which sets how prominent the entry is and
whether it reads as prose.

The narrator's default must be to emit /nothing/: measured over 3,111 real
messages, about 64% are pure plumbing (@Do@, @CheckWindows@, @ClearUI@,
@Ask@...). There is deliberately no "unknown" kind to fall back on.
-}
data LogKind
  = -- | A heading: "Round 2", "Mythos phase", "Daisy's turn".
    Structure
  | -- | A player chose to do something.
    Action
  | -- | An engine consequence: damage, clues, a spawn, a draw.
    Mechanic
  | -- | A skill test, as one group.
    Test
  | -- | Flavour or story text, rendered as prose.
    Narrative
  | -- | A write to the campaign or scenario log.
    Record
  | -- | "ignored", "cannot", "no effect" -- true but subordinate.
    Notice
  | -- | An error, or a custom card that did not do what it said.
    Problem
  | {- | Something a player typed. Not an engine event at all, which is exactly
    why it has its own kind: it is drawn as a quote, it is never terse, and
    nothing should ever try to derive one.
    -}
    Chat
  deriving stock (Show, Eq, Ord, Data)

{- | Who may see an entry.

Today hidden information cannot be logged at all: @toClientText@ maps every
'Arkham.Classes.GameLogger.ClientCardOnly' to 'Nothing', so "you drew an
enemy" is a live popup that never reaches history. Carrying the audience on
the row is what lets those events persist, filtered per seat on read.
-}
data LogAudience
  = Everyone
  | OnlyPlayer PlayerId
  deriving stock (Show, Eq, Ord, Data)

{- | Whether an entry went well or badly for the investigators.

Separate from 'LogKind', which says what sort of event it was. A skill test is
a 'Test' whether it passed or failed; the tone is what lets the client colour
the result band without parsing the sentence back out of its i18n key.
-}
data LogTone = Good | Bad
  deriving stock (Show, Eq, Ord, Data)

{- | Which group an entry belongs to, and what part it plays in it.

The log is a flat, append-only list; a group is a rendering concept. Every
entry a skill test produces carries that test's id, and the client draws
consecutive entries sharing an id as one block. Nothing is ever rewritten and
nothing is nested server-side, which is what keeps undo honest: rows are
deleted by step like everything else, so undoing into the middle of a test
removes exactly the entries from the undone steps and leaves the rest of the
group standing.

An entry with no group is an ordinary line. Anything a player types is
deliberately ungrouped, so chat during a test lands below the block rather than
inside it.
-}
data LogGroupRole
  = -- | Opens the group and names it: "Daisy Walker is investigating the Study".
    GroupHeader
  | -- | A line inside the group.
    GroupMember
  | {- | Closes the group with its outcome. This is the band the block collapses
    to, so it should read on its own.
    -}
    GroupSummary
  deriving stock (Show, Eq, Ord, Data)

data LogGroup = LogGroup
  { logGroupId :: Text
  , logGroupRole :: LogGroupRole
  }
  deriving stock (Show, Eq, Ord, Data)

{- | Where in the game an entry happened, for a reader scrolling back through
history without the surrounding structure lines in view.
-}
data LogContext = LogContext
  { logContextRound :: Maybe Int
  , logContextPhase :: Maybe Text
  , logContextTurn :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Ord, Data)

emptyLogContext :: LogContext
emptyLogContext = LogContext Nothing Nothing Nothing

{- | One entry, possibly holding others.

'logEntryChildren' is what lets the log cover a composite event as a single
readable unit: a skill test is one entry holding its commits, revealed tokens,
modifiers and result, rather than eight flat lines the reader has to
reassemble.

'logEntrySeq' is stamped by the transport, not by whoever builds the entry, and
is only meaningful on a top-level entry -- children are addressed by their
index path beneath it (@"7.0.1"@), which is also how the client keys its
open/closed state. 'mkLogEntry' leaves it at 0.

'logEntryStep' is stamped the same way, and is the game step the entry was
written under -- which is the step an undo has to land on to put the game back
to just before it. Every entry an action produced shares one step, so undoing
to it reverts that whole action, not half of it. 'Nothing' means the entry has
not been through the transport yet (or came from a row written before the
column existed), and the client simply offers no undo for it.
-}
data LogEntry = LogEntry
  { logEntrySeq :: Int
  , logEntryStep :: Maybe Int
  , logEntryKind :: LogKind
  , logEntryBody :: [LogPart]
  , logEntrySource :: Maybe LogRef
  {- ^ What caused this. Answers "why" as data, so the author never has to
  write the cause into the sentence.
  -}
  , logEntryChildren :: [LogEntry]
  , logEntryAudience :: LogAudience
  , logEntryContext :: LogContext
  , logEntryTone :: Maybe LogTone
  -- ^ How it turned out, where that is worth showing rather than only saying.
  , logEntryGroup :: Maybe LogGroup
  -- ^ The block this line belongs to, if any. See 'LogGroup'.
  , logEntryTag :: Maybe Text
  {- ^ A name the engine can use to take this entry back later.

  Only for something that can be undone by play rather than by undo -- a card
  committed to a test and then uncommitted. The tag has to identify the thing,
  not the line: @"commit:\<cardId>"@, so the retraction can name it without
  having kept a handle on the entry.
  -}
  }
  deriving stock (Show, Eq, Ord, Data)

-- | A bare entry of the given kind. Seq is stamped later; see 'LogEntry'.
mkLogEntry :: LogKind -> [LogPart] -> LogEntry
mkLogEntry kind body =
  LogEntry
    { logEntrySeq = 0
    , logEntryStep = Nothing
    , logEntryKind = kind
    , logEntryBody = body
    , logEntrySource = Nothing
    , logEntryChildren = []
    , logEntryAudience = Everyone
    , logEntryContext = emptyLogContext
    , logEntryTone = Nothing
    , logEntryGroup = Nothing
    , logEntryTag = Nothing
    }

{- | Total rows an entry occupies when fully expanded, which is what the
client's "+N" badge counts.
-}
logEntrySize :: LogEntry -> Int
logEntrySize e = 1 + sum (map logEntrySize e.logEntryChildren)

{- | Stamp a run of top-level entries with consecutive sequence numbers.
Children are left alone; see 'LogEntry'.
-}
stampLogEntries :: Int -> [LogEntry] -> [LogEntry]
stampLogEntries start = zipWith (\n e -> e {logEntrySeq = n}) [start ..]

-- | Tag an entry with the game step it was written under. See 'LogEntry'.
atLogStep :: Int -> LogEntry -> LogEntry
atLogStep step e = e {logEntryStep = Just step}

{- | One row of a game's log as it goes to the client.

Two shapes because history is mixed: rows written before the overhaul have only
a flat brace-DSL @body@, and the parser for that format lives in the client
(@legacyLogParse.ts@), run once at ingest. Writing a throwaway Haskell copy of a
format that is being deleted would buy nothing, and the one case that needs game
state -- whether a location is revealed -- is available there.

So the server says which shape each row is and the client normalises both into
the same parts. That is what lets @GameMessage.vue@ be deleted outright instead
of surviving as a second render path.

A legacy row carries the game step beside its body for the same reason a
structured entry carries it inside: it is what "undo to here" needs, and most
of a live game's scrollback is still legacy.
-}
data LogRow
  = LogRowStructured LogEntry
  | LogRowLegacy Text (Maybe Int)
  deriving stock (Show, Eq, Ord, Data)

{- | Plain text for a context that has no renderer: the replay tool's trace, a
server-side error, a test assertion. Lossy on purpose -- refs become their
names and an i18n key becomes the key, because there is no locale here.
-}
logPartToText :: LogPart -> Text
logPartToText = \case
  LogText t -> t
  LogI18n k vars
    | Map.null vars -> "$" <> k
    | otherwise ->
        "$" <> k <> "(" <> intercalate ", " [v <> "=" <> logPartToText p | (v, p) <- Map.toList vars] <> ")"
  LogRefPart r -> r.logRefName
  LogNumber n -> tshow n
  LogDelta n -> (if n < 0 then "" else "+") <> tshow n
  LogToken t -> "[" <> t <> "]"
  LogIcon t -> "[" <> t <> "]"
  {- The key itself. This rendering has no locale, and the campaign-log key's
  display name lives entirely on the client, so the raw key is the honest
  fallback for a trace or an error message. -}
  LogCampaignKey v -> tshow v
  LogList ps -> intercalate ", " (map logPartToText ps)

-- | The entry as one line of plain text, children excluded.
logEntryToText :: LogEntry -> Text
logEntryToText = mconcat . map logPartToText . logEntryBody

-- | Short tag for a kind, for traces and test output.
logKindTag :: LogKind -> Text
logKindTag = \case
  Structure -> "structure"
  Action -> "action"
  Mechanic -> "mechanic"
  Test -> "test"
  Narrative -> "narrative"
  Record -> "record"
  Notice -> "notice"
  Problem -> "problem"
  Chat -> "chat"

{- | The entry and its children as indented lines.

This is what @arkham-replay --trace@ prints and what a test asserts on: the
whole narrative of a scenario as a diff-able block of text. Reading it is how
you tell whether the log actually explains what happened.
-}
logEntryToLines :: LogEntry -> [Text]
logEntryToLines = go 0
 where
  go d e =
    ( T.replicate d "  "
        <> "["
        <> logKindTag e.logEntryKind
        <> "] "
        <> logEntryToText e
        <> maybe "" (\r -> " <- " <> r.logRefName) e.logEntrySource
        <> audienceSuffix e.logEntryAudience
    )
      : concatMap (go (d + 1)) e.logEntryChildren
  audienceSuffix = \case
    Everyone -> ""
    OnlyPlayer pid -> " (only " <> tshow pid <> ")"

$(deriveJSON defaultOptions ''LogRefKind)
$(deriveJSON (aesonOptions $ Just "logRef") ''LogRef)
$(deriveJSON defaultOptions ''LogPart)
$(deriveJSON defaultOptions ''LogKind)
$(deriveJSON defaultOptions ''LogAudience)
$(deriveJSON defaultOptions ''LogTone)
$(deriveJSON defaultOptions ''LogGroupRole)
$(deriveJSON (aesonOptions $ Just "logGroup") ''LogGroup)
$(deriveJSON (aesonOptions $ Just "logContext") ''LogContext)
$(deriveJSON (aesonOptions $ Just "logEntry") ''LogEntry)
$(deriveJSON defaultOptions ''LogRow)

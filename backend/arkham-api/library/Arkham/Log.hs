{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | Building and sending game-log entries.

The typed layer over "Arkham.Log.Entry". That module is a leaf (everything
imports "Arkham.Classes.GameLogger", which imports it), so it carries names and
token faces as 'Text'; this module sits above the game types and is what
callers actually use.

What replaced what:

> -- before: two string DSLs, composed by (<>), escaped by hand
> send $ format (toCard attrs) <> " removed all copies of " <> format card <> " from the game"
>
> -- after: parts, so the client gets chips and the sentence can be localized
> sendLog $ mechanic ["card" .= toCard attrs, "removed" .= card] ...

See @docs/game-log/@ for the design and the journal. Nesting, grouping and
resolving a 'Arkham.Source.Source' to a ref all need game state and arrive with
the narrator in a later phase; this module is deliberately pure.
-}
module Arkham.Log (module Arkham.Log, module Arkham.Log.Entry) where

import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.GameLogger
import Arkham.I18n
import Arkham.Id
import Arkham.Log.Entry
import Arkham.Name
import Arkham.Prelude
import Arkham.SkillType
import Data.Aeson (Result (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- * Parts

{- | Anything that can stand in a log sentence.

Deliberately NOT a replacement for 'Arkham.Classes.GameLogger.ToGameLoggerFormat':
that class returns 'Text' and is what produced the brace DSL. This one returns
structure, so the client decides how to draw it.
-}
class ToLogPart a where
  toLogPart :: a -> LogPart

instance ToLogPart LogPart where
  toLogPart = id

instance ToLogPart Text where
  toLogPart = LogText

instance ToLogPart Int where
  toLogPart = LogNumber

instance ToLogPart LogRef where
  toLogPart = LogRefPart

instance ToLogPart Name where
  toLogPart = LogText . display

instance ToLogPart Card where
  toLogPart = LogRefPart . toLogRef

instance ToLogPart SkillType where
  toLogPart = LogIcon . skillTypeKey

instance ToLogPart ChaosTokenFace where
  toLogPart = LogToken . chaosTokenFaceKey

instance ToLogPart ChaosToken where
  toLogPart = toLogPart . chaosTokenFace

instance ToLogPart a => ToLogPart [a] where
  toLogPart = LogList . map toLogPart

{- | A literal. Reach for 'ikeyPart' instead wherever the text is player-facing:
English in Haskell is what left the log untranslatable.
-}
lit :: Text -> LogPart
lit = LogText

num :: Int -> LogPart
num = LogNumber

-- | A signed modifier, drawn @+2@ / @-1@.
delta :: Int -> LogPart
delta = LogDelta

{- | An i18n template with named variables, each of which may itself be rich.

> ikeyPart "discoveredClues" ["investigator" ~> iid', "location" ~> lid', "count" ~> n]
-}
ikeyPart :: Text -> [(Text, LogPart)] -> LogPart
ikeyPart k = LogI18n k . Map.fromList

{- | An i18n part whose key is resolved against the ambient @?scope@, and which
picks up whatever @withVar@ / @countVar@ put in @?scopeVars@.

This is the bridge for __campaign and scenario authors__, who write inside a
@scenarioI18n@ / @withI18n@ block and expect a short key:

> scenarioI18n $ withVar "card" (String "Shrine") $ sendLogI18n Narrative "message.banished"

'ikeyPart' is the raw-key version, for the narrator and anything that already
knows the full key.
-}
ikeyScoped :: HasI18n => Scope -> [(Text, LogPart)] -> LogPart
ikeyScoped k vars =
  LogI18n (T.intercalate "." (?scope <> [k])) (scopeVarParts <> Map.fromList vars)

{- | The ambient @?scopeVars@ as parts, so the values a scenario set with
@withVar@ / @countVar@ reach the client. Explicit vars passed alongside win.
-}
scopeVarParts :: HasI18n => Map Text LogPart
scopeVarParts = Map.map valueToPart ?scopeVars
 where
  -- Via aeson's own Int parser rather than Data.Scientific, which is not a
  -- declared dependency of this package.
  valueToPart v = case v of
    String t -> LogText t
    Number _ | Success (i :: Int) <- fromJSON v -> LogNumber i
    _ -> LogText (tshow v)

{- | Send one scoped entry. The short form a scenario reaches for.

Direct sending is a first-class path, not a leftover: the narrator covers what
the engine can infer, and a campaign's one-off moments -- a specific card being
banished, a resident refusing to speak -- are exactly what it cannot.
-}
sendLogI18n :: (HasI18n, HasGameLogger m) => LogKind -> Scope -> m ()
sendLogI18n kind k = sendLog $ mkLogEntry kind [ikeyScoped k []]

-- | Pair a variable name with its part, for 'ikeyPart'.
(~>) :: ToLogPart a => Text -> a -> (Text, LogPart)
k ~> a = (k, toLogPart a)

infixr 6 ~>

{- | The wire name for a skill icon. Matches the token the client already uses
for skill colours.
-}
skillTypeKey :: SkillType -> Text
skillTypeKey = \case
  SkillWillpower -> "willpower"
  SkillIntellect -> "intellect"
  SkillCombat -> "combat"
  SkillAgility -> "agility"

{- | The wire name for a chaos token face.

Same rule as 'Arkham.ChaosToken.Types.format': a custom token is its homebrew
slug, anything else is its constructor. Kept identical on purpose, so the
client's existing token-image lookup needs no change -- and note that the old
DSL double-quoted a @CustomToken@ into @{token:"CustomToken \"slug\"\"}@, which
@GameMessage.vue@ still patches up at render time. Structure makes that
unrepresentable.
-}
chaosTokenFaceKey :: ChaosTokenFace -> Text
chaosTokenFaceKey = \case
  CustomToken slug -> slug
  face -> tshow face

-- * Refs

-- | Something that can be pointed at from the log.
class ToLogRef a where
  toLogRef :: a -> LogRef

instance ToLogRef LogRef where
  toLogRef = id

instance ToLogRef Card where
  toLogRef c =
    (logRef RefCard (display $ toName c))
      { logRefCardCode = Just (toCardCode c)
      , logRefCardId = Just (toCardId c)
      }

{- | An id as the client sees it: its JSON encoding, never its 'Show'.

'Show' is wrong for any id wrapping 'Text'. @CardCode@ derives @Show@ from
@Text@, so @tshow (InvestigatorId "01002")@ is @"\\"01002\\""@ -- quotes
included -- and that shipped once: 'logRefEntityId' came out doubly quoted. The
old brace DSL got away with it only because the spurious quotes doubled as its
own field delimiters.

The JSON form is also exactly what keys @game.investigators@, @game.enemies@ and
friends on the client, so a ref built this way can be looked up there with no
massaging.
-}
idText :: ToJSON a => a -> Text
idText a = case toJSON a of
  String t -> t
  -- Unreachable: every id in "Arkham.Id" encodes as a JSON string.
  other -> tshow other

{- | A ref carrying an in-play entity's id alongside its card code, which is
what lets the client prefer the live entity's current face.
-}
entityRef :: ToJSON i => LogRefKind -> Text -> i -> CardCode -> LogRef
entityRef kind name eid code =
  (logRef kind name) {logRefEntityId = Just (idText eid), logRefCardCode = Just code}

{- | An investigator ref built from the id alone, for a caller that has no name
to hand -- the narrator, which reads only messages.

The name is left as the card code because the client resolves the display name
from 'logRefCardCode' anyway, and does it better: it localizes, and it follows
a transformed investigator (a Yithian body, say) that a name captured
server-side at narration time would not. 'logRefName' is the fallback for a
client that cannot look it up.
-}
investigatorRefById :: InvestigatorId -> LogRef
investigatorRefById iid =
  (logRef RefInvestigator (unCardCode $ unInvestigatorId iid))
    { logRefEntityId = Just (idText iid)
    , logRefCardCode = Just (unInvestigatorId iid)
    }

investigatorRef :: Named name => InvestigatorId -> name -> LogRef
investigatorRef iid name =
  (logRef RefInvestigator (display $ toName name))
    { logRefEntityId = Just (idText iid)
    , logRefCardCode = Just (unInvestigatorId iid)
    }

enemyRef :: Named name => EnemyId -> name -> CardCode -> LogRef
enemyRef eid name = entityRef RefEnemy (display $ toName name) eid

{- | A location. 'logRefFaceDown' is set from @revealed@, because an unrevealed
location must draw its back -- the old renderer reached into @game.locations@
from inside a render function to work that out (@GameMessage.vue:50-53@).
-}
locationRef :: Named name => LocationId -> name -> CardCode -> Bool -> LogRef
locationRef lid name code revealed =
  (entityRef RefLocation (display $ toName name) lid code)
    { logRefFaceDown = not revealed
    }

assetRef :: Named name => AssetId -> name -> CardCode -> LogRef
assetRef aid name = entityRef RefAsset (display $ toName name) aid

treacheryRef :: Named name => TreacheryId -> name -> CardCode -> LogRef
treacheryRef tid name = entityRef RefTreachery (display $ toName name) tid

eventRef :: Named name => EventId -> name -> CardCode -> LogRef
eventRef eid name = entityRef RefEvent (display $ toName name) eid

actRef :: Named name => ActId -> name -> CardCode -> LogRef
actRef aid name = entityRef RefAct (display $ toName name) aid

agendaRef :: Named name => AgendaId -> name -> CardCode -> LogRef
agendaRef aid name = entityRef RefAgenda (display $ toName name) aid

{- | The scenario itself, for the banner that opens one. Its id IS its card
code, so the client resolves a localized title and the art to hover; the name
is the fallback a homebrew scenario needs, since it is in nobody's card index.
-}
scenarioRef :: Named name => ScenarioId -> name -> LogRef
scenarioRef sid name = entityRef RefScenario (display $ toName name) sid (unScenarioId sid)

storyRef :: Named name => StoryId -> name -> LogRef
storyRef sid name =
  (logRef RefStory (display $ toName name)) {logRefEntityId = Just (idText sid)}

-- * Entries

-- | A heading: "Round 2", "Mythos phase".
structure :: [LogPart] -> LogEntry
structure = mkLogEntry Structure

-- | A player chose to do something.
action :: [LogPart] -> LogEntry
action = mkLogEntry Action

-- | An engine consequence: damage, clues, a spawn, a draw.
mechanic :: [LogPart] -> LogEntry
mechanic = mkLogEntry Mechanic

-- | A skill test, as one group.
test :: [LogPart] -> LogEntry
test = mkLogEntry Test

-- | Flavour or story text, rendered as prose.
narrative :: [LogPart] -> LogEntry
narrative = mkLogEntry Narrative

-- | A write to the campaign or scenario log.
record :: [LogPart] -> LogEntry
record = mkLogEntry Record

-- | True but subordinate: "ignored", "cannot", "no effect".
notice :: [LogPart] -> LogEntry
notice = mkLogEntry Notice

-- | An error, or a custom card that did not do what it said.
problem :: [LogPart] -> LogEntry
problem = mkLogEntry Problem

-- | Something a player typed into the log's chat box.
chat :: [LogPart] -> LogEntry
chat = mkLogEntry Chat

-- | How an entry turned out, for a client that draws the outcome.
toned :: LogTone -> LogEntry -> LogEntry
toned t e = e {logEntryTone = Just t}

{- | Put an entry in a group, so the client draws it as part of that block.
See 'LogGroup'.
-}
inGroup :: Text -> LogGroupRole -> LogEntry -> LogEntry
inGroup gid role e = e {logEntryGroup = Just (LogGroup gid role)}

-- | Opens a block and names it.
opensGroup :: Text -> LogEntry -> LogEntry
opensGroup gid = inGroup gid GroupHeader

-- | A line inside a block.
inGroupOf :: Text -> LogEntry -> LogEntry
inGroupOf gid = inGroup gid GroupMember

-- | Closes a block with its outcome; this is what it collapses to.
closesGroup :: Text -> LogEntry -> LogEntry
closesGroup gid = inGroup gid GroupSummary

-- | Attach detail beneath an entry. One composite event, one readable unit.
withChildren :: [LogEntry] -> LogEntry -> LogEntry
withChildren cs e = e {logEntryChildren = e.logEntryChildren <> cs}

-- | Name what caused an entry, as data rather than as prose.
because :: ToLogRef a => a -> LogEntry -> LogEntry
because a e = e {logEntrySource = Just (toLogRef a)}

{- | Restrict an entry to one seat. This is the only way hidden information can
enter history at all; see 'LogAudience'.
-}
forPlayer :: PlayerId -> LogEntry -> LogEntry
forPlayer pid e = e {logEntryAudience = OnlyPlayer pid}

-- * Sending

{- | Name an entry so it can be taken back later with 'retractLog'. See
'logEntryTag'.
-}
tagged :: Text -> LogEntry -> LogEntry
tagged t e = e {logEntryTag = Just t}

{- | Take back every entry carrying this tag.

For a thing the game said and then unsaid. Harmless when nothing matches, so a
caller does not have to know whether the line was ever written.
-}
retractLog :: HasGameLogger m => Text -> m ()
retractLog tag = do
  f <- getLogger
  liftIO $ f (ClientRetractLog tag)

-- | Send one entry. The transport stamps 'logEntrySeq' and batches the frame.
sendLog :: HasGameLogger m => LogEntry -> m ()
sendLog entry = do
  f <- getLogger
  liftIO $ f (ClientLogEntry entry)

-- | Send several, in order.
sendLogs :: HasGameLogger m => [LogEntry] -> m ()
sendLogs = traverse_ sendLog

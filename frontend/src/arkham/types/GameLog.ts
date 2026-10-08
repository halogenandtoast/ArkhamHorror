import * as JsonDecoder from 'ts.data.json'
import { logKeyDecoder, type LogKey } from '@/arkham/types/Log';

/* The structured game log.
 *
 * Mirrors Arkham.Log.Entry on the backend. See docs/game-log/ for the design.
 *
 * What this replaces: entries used to arrive as a flat string carrying a brace
 * mini-language (`{card:"name":CODE:"id"}`) with an embedded i18n DSL spliced
 * into it, and GameMessage.vue re-parsed them with seven regexes inside a
 * render function, on every render. Parts arrive already parsed, so rendering
 * is a v-for.
 */

export type LogRefKind =
  | 'RefCard'
  | 'RefInvestigator'
  | 'RefEnemy'
  | 'RefLocation'
  | 'RefAsset'
  | 'RefEvent'
  | 'RefSkill'
  | 'RefTreachery'
  | 'RefAct'
  | 'RefAgenda'
  | 'RefStory'
  | 'RefScenario'
  | 'RefAbility'

/* A pointer to something in the game, drawn as a chip the reader can hover.
 *
 * One record with a `kind`, rather than a constructor per kind: the old DSL
 * grew a separate shape per kind and then a second *arity* for locations, which
 * the renderer had to try in order. A new kind now changes nothing here.
 */
export interface LogRef {
  kind: LogRefKind
  name: string
  cardCode: string | null
  cardId: string | null
  entityId: string | null
  faceDown: boolean
}

export type LogPart =
  | { tag: 'LogText'; contents: string }
  /* An i18n key plus named variables, each of which may itself be a part. This
   * is the pairing the old system could not express: a localized template that
   * still carries card chips. */
  | { tag: 'LogI18n'; contents: [string, Record<string, LogPart>] }
  | { tag: 'LogRefPart'; contents: LogRef }
  | { tag: 'LogNumber'; contents: number }
  /* Rendered +2 / -1 rather than as a bare number. */
  | { tag: 'LogDelta'; contents: number }
  | { tag: 'LogToken'; contents: string }
  | { tag: 'LogIcon'; contents: string }
  /* Joined by the locale's list rule, so "a, b, and c" is not built in Haskell. */
  /* A campaign-log key as its own JSON. The client owns the mapping from a key
   * to its i18n path (formatKey in types/Log.ts), so the server sends the key
   * rather than a path it would have to reimplement. */
  | { tag: 'LogCampaignKey'; contents: LogKey }
  | { tag: 'LogList'; contents: LogPart[] }

export type LogKind =
  | 'Structure'
  | 'Action'
  | 'Mechanic'
  | 'Test'
  | 'Narrative'
  | 'Record'
  | 'Notice'
  | 'Problem'
  /* Something a player typed. Never derived; drawn as a quote. */
  | 'Chat'

export type LogAudience =
  | { tag: 'Everyone' }
  | { tag: 'OnlyPlayer'; contents: string }

/* How an entry turned out, where that is worth showing rather than only saying.
 * Separate from LogKind, which says what sort of event it was: a skill test is
 * a Test whether it passed or failed. */
export type LogTone = 'Good' | 'Bad'

export type LogGroupRole = 'GroupHeader' | 'GroupMember' | 'GroupSummary'

export interface LogGroup {
  id: string
  role: LogGroupRole
}

export interface LogContext {
  round: number | null
  phase: string | null
  turn: string | null
}

export interface LogEntry {
  /* Monotonic per game, stamped by the server. Meaningful on a top-level entry;
   * children are addressed by their index path beneath it ("7.0.1"), which is
   * also how the open/closed state is keyed. */
  seq: number
  /* The game step this entry was written under, which is the step an undo has
   * to land on to put the game back to just before it. Every entry one action
   * produced shares a step. Null means the entry has no undo target. */
  step: number | null
  kind: LogKind
  body: LogPart[]
  /* What caused this, as data rather than written into the sentence. */
  source: LogRef | null
  children: LogEntry[]
  audience: LogAudience
  context: LogContext
  tone: LogTone | null
  /* A stable identity for an entry that is written once and then revised — a
   * skill test's block, which opens when the test begins and is rewritten as it
   * proceeds. The server does the merging; this is here so the client can key
   * the list by it rather than by position. */
  /* The block this line belongs to, if any.
   *
   * The log is a flat, append-only list; a block is a rendering concept. Every
   * entry a skill test produces carries that test's id, and consecutive entries
   * sharing an id are drawn as one block. Nothing is rewritten server-side,
   * which is what keeps undo honest. */
  group: LogGroup | null
}

export const logRefKindDecoder = JsonDecoder.oneOf<LogRefKind>(
  [
    'RefCard',
    'RefInvestigator',
    'RefEnemy',
    'RefLocation',
    'RefAsset',
    'RefEvent',
    'RefSkill',
    'RefTreachery',
    'RefAct',
    'RefAgenda',
    'RefStory',
    'RefScenario',
    'RefAbility',
  ].map((k) => JsonDecoder.literal(k) as JsonDecoder.Decoder<LogRefKind>),
  'LogRefKind',
)

export const logRefDecoder = JsonDecoder.object<LogRef>(
  {
    kind: logRefKindDecoder,
    name: JsonDecoder.string(),
    cardCode: JsonDecoder.nullable(JsonDecoder.string()),
    cardId: JsonDecoder.nullable(JsonDecoder.string()),
    entityId: JsonDecoder.nullable(JsonDecoder.string()),
    faceDown: JsonDecoder.boolean(),
  },
  'LogRef',
)

export const logPartDecoder: JsonDecoder.Decoder<LogPart> = JsonDecoder.oneOf<LogPart>(
  [
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogText'), contents: JsonDecoder.string() },
      'LogText',
    ),
    JsonDecoder.object(
      {
        tag: JsonDecoder.literal('LogI18n'),
        contents: JsonDecoder.tuple(
          [
            JsonDecoder.string(),
            JsonDecoder.dictionary(
              JsonDecoder.lazy(() => logPartDecoder),
              'LogI18nVars',
            ),
          ],
          'LogI18nContents',
        ),
      },
      'LogI18n',
    ),
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogRefPart'), contents: logRefDecoder },
      'LogRefPart',
    ),
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogNumber'), contents: JsonDecoder.number() },
      'LogNumber',
    ),
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogDelta'), contents: JsonDecoder.number() },
      'LogDelta',
    ),
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogToken'), contents: JsonDecoder.string() },
      'LogToken',
    ),
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogIcon'), contents: JsonDecoder.string() },
      'LogIcon',
    ),
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogCampaignKey'), contents: logKeyDecoder },
      'LogCampaignKey',
    ),
    JsonDecoder.object(
      {
        tag: JsonDecoder.literal('LogList'),
        contents: JsonDecoder.array(
          JsonDecoder.lazy(() => logPartDecoder),
          'LogPart[]',
        ),
      },
      'LogList',
    ),
  ],
  'LogPart',
)

export const logKindDecoder = JsonDecoder.oneOf<LogKind>(
  ['Structure', 'Action', 'Mechanic', 'Test', 'Narrative', 'Record', 'Notice', 'Problem', 'Chat'].map(
    (k) => JsonDecoder.literal(k) as JsonDecoder.Decoder<LogKind>,
  ),
  'LogKind',
)

export const logAudienceDecoder = JsonDecoder.oneOf<LogAudience>(
  [
    JsonDecoder.object({ tag: JsonDecoder.literal('Everyone') }, 'Everyone'),
    JsonDecoder.object(
      { tag: JsonDecoder.literal('OnlyPlayer'), contents: JsonDecoder.string() },
      'OnlyPlayer',
    ),
  ],
  'LogAudience',
)

export const logContextDecoder = JsonDecoder.object<LogContext>(
  {
    round: JsonDecoder.nullable(JsonDecoder.number()),
    phase: JsonDecoder.nullable(JsonDecoder.string()),
    turn: JsonDecoder.nullable(JsonDecoder.string()),
  },
  'LogContext',
)

export const logGroupDecoder = JsonDecoder.object<LogGroup>(
  {
    id: JsonDecoder.string(),
    role: JsonDecoder.oneOf<LogGroupRole>(
      [
        JsonDecoder.literal('GroupHeader'),
        JsonDecoder.literal('GroupMember'),
        JsonDecoder.literal('GroupSummary'),
      ] as JsonDecoder.Decoder<LogGroupRole>[],
      'LogGroupRole',
    ),
  },
  'LogGroup',
)

export const logEntryDecoder: JsonDecoder.Decoder<LogEntry> = JsonDecoder.object<LogEntry>(
  {
    seq: JsonDecoder.number(),
    /* failover, not nullable: a server that predates the field sends no key at
       all, and a decode failure here takes the whole log down with it. */
    step: JsonDecoder.failover<number | null>(null, JsonDecoder.nullable(JsonDecoder.number())),
    kind: logKindDecoder,
    body: JsonDecoder.array(logPartDecoder, 'LogPart[]'),
    source: JsonDecoder.nullable(logRefDecoder),
    children: JsonDecoder.array(
      JsonDecoder.lazy(() => logEntryDecoder),
      'LogEntry[]',
    ),
    audience: logAudienceDecoder,
    context: logContextDecoder,
    /* failover for the same reason as `step`: a server one build behind sends
       no key, and a strict decoder would take the whole log down with it. */
    group: JsonDecoder.failover<LogGroup | null>(null, JsonDecoder.nullable(logGroupDecoder)),
    tone: JsonDecoder.failover<LogTone | null>(
      null,
      JsonDecoder.nullable(
        JsonDecoder.oneOf<LogTone>(
          [JsonDecoder.literal('Good'), JsonDecoder.literal('Bad')] as JsonDecoder.Decoder<LogTone>[],
          'LogTone',
        ),
      ),
    ),
  },
  'LogEntry',
)

/* The log as the renderer wants it: a flat run of entries becomes a run of
 * items, where entries sharing a group id collapse into one block.
 *
 * Keyed by id rather than by adjacency, because a block's rows are NOT
 * guaranteed to be contiguous: a card play's own aftermath can arrive after an
 * unrelated event (in one real log, a treachery's header at seq 77 and its
 * discard at 82, with a whole skill test between). Adjacency split that into
 * two blocks, and the half holding only members had neither a header nor a
 * summary to draw -- an empty, invisible row.
 *
 * Grouping is done here rather than on the server so that the stored log stays
 * append-only -- rows are deleted by step on undo and nothing is ever rewritten,
 * which is what makes undoing into the middle of a block behave sensibly: the
 * entries from the undone steps go and the rest of the block stays. */
export type LogItem =
  | { kind: 'entry'; entry: LogEntry }
  | { kind: 'group'; id: string; header: LogEntry | null; members: LogEntry[]; summary: LogEntry | null }

export function groupLogEntries(entries: readonly LogEntry[]): LogItem[] {
  const items: LogItem[] = []
  const groups = new Map<string, Extract<LogItem, { kind: 'group' }>>()
  for (const entry of entries) {
    const gid = entry.group?.id
    if (!gid) {
      items.push({ kind: 'entry', entry })
      continue
    }
    let group = groups.get(gid)
    if (!group) {
      group = { kind: 'group', id: gid, header: null, members: [], summary: null }
      groups.set(gid, group)
      items.push(group)
    }
    switch (entry.group?.role) {
      case 'GroupHeader':
        group.header = entry
        break
      case 'GroupSummary':
        group.summary = entry
        break
      default:
        group.members.push(entry)
    }
  }
  /* A block with no bar of its own has nothing to collapse to, so its members
     render as ordinary lines instead of as a box the reader cannot open. That
     happens legitimately: an undo deletes by step, and a card play's header is
     written LAST, so undoing one step can leave its members behind. */
  return items.flatMap(item =>
    item.kind === 'group' && !item.header && !item.summary
      ? item.members.map((entry): LogItem => ({ kind: 'entry', entry }))
      : [item],
  )
}

/* Rows an entry occupies when fully expanded: what the "+N" badge counts. */
export function logEntrySize(entry: LogEntry): number {
  return entry.children.reduce((acc, c) => acc + logEntrySize(c), 1)
}

export function hasChildren(entry: LogEntry): boolean {
  return entry.children.length > 0
}

/* One row of a game's log as it arrives.
 *
 * Mixed on purpose: rows written before the overhaul carry only their flat
 * brace-DSL body, and the parser for that format lives in legacyLogParse.ts,
 * run once at ingest. The server says which shape each row is; the client
 * normalises both into the same entries.
 */
export type LogRow =
  | { tag: 'LogRowStructured'; contents: LogEntry }
  /* [body, step] -- a legacy row carries its step beside the text so that
   * "undo to here" works on the scrollback of a game that predates the
   * structured log, which is most of them. */
  | { tag: 'LogRowLegacy'; contents: [string, number | null] }

export const logRowDecoder = JsonDecoder.oneOf<LogRow>(
  [
    JsonDecoder.object(
      { tag: JsonDecoder.literal('LogRowStructured'), contents: logEntryDecoder },
      'LogRowStructured',
    ),
    JsonDecoder.object(
      {
        tag: JsonDecoder.literal('LogRowLegacy'),
        /* Same reason: an older server sends a bare string here. */
        contents: JsonDecoder.failover<[string, number | null]>(
          ['', null],
          JsonDecoder.oneOf<[string, number | null]>(
            [
              JsonDecoder.tuple(
                [JsonDecoder.string(), JsonDecoder.nullable(JsonDecoder.number())],
                'LogRowLegacyContents',
              ),
              JsonDecoder.string().map((body) => [body, null] as [string, number | null]),
            ],
            'LogRowLegacyContents',
          ),
        ),
      },
      'LogRowLegacy',
    ),
  ],
  'LogRow',
)

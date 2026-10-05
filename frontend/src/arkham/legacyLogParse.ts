import { parseInput } from '@/arkham/i18n'
import type { LogEntry, LogPart, LogRef, LogRefKind, LogRow } from '@/arkham/types/GameLog'

/* Parsing the old flat log format into parts.
 *
 * Every log row written before the overhaul is a single string carrying a brace
 * mini-language (`{card:"name":CODE:"id"}`, `{investigator:…}`, `{token:…}`)
 * with an embedded i18n DSL (`$key var=s:"…"`) spliced into it. This converts
 * one into the same `LogPart[]` the server now sends.
 *
 * It runs ONCE, at ingest, never in a render function. That is the whole point:
 * GameMessage.vue did this work with seven regex test+match pairs per fragment
 * on every render, and three of those branches (enemy, and both location
 * arities) matched nothing at all in a sampled 481-entry campaign. Keeping the
 * parse here means old games still read correctly while the renderer has
 * exactly one code path.
 *
 * Do not extend this for new behaviour. The format it parses is closed.
 */

/* Minimum the location lookup needs; Game's own type is far wider. */
export interface LegacyLocationLookup {
  [id: string]: { cardCode: string; revealed: boolean } | undefined
}

const REF_SPLIT = /({[^}]+})/
const CARD = /^{card:"((?:[^"]|\\.)+)":"?([^":]+)"?:"([^"]+)"}$/
const INVESTIGATOR = /^{investigator:"((?:[^"]|\\.)+)":"([^"]+)"}$/
const ENEMY = /^{enemy:"((?:[^"]|\\.)+)":(.+):"([^"]+)"}$/
const LOCATION_WITH_CODE = /^{location:"((?:[^"]|\\.)+)":(.+):"([^"]+)"}$/
const LOCATION = /^{location:"((?:[^"]|\\.)+)":(.+)}$/
const TOKEN = /^{token:"([^"]+)"}$/
const I18N_TOKEN = /(\$[A-Za-z0-9_.]+)/

const unescapeQuotes = (s: string) => s.replace(/\\"/g, '"')

function ref(kind: LogRefKind, name: string, over: Partial<LogRef> = {}): LogPart {
  return {
    tag: 'LogRefPart',
    contents: {
      kind,
      name: unescapeQuotes(name),
      cardCode: null,
      cardId: null,
      entityId: null,
      faceDown: false,
      ...over,
    },
  }
}

/* An i18n token with its parameters, as a part rather than as pre-rendered text.
 * Mirrors what the server now emits directly. */
function i18nPart(body: string): LogPart {
  const { key, params } = parseInput(body)
  const vars: Record<string, LogPart> = {}
  for (const [k, v] of Object.entries(params ?? {})) {
    vars[k] = typeof v === 'number' ? { tag: 'LogNumber', contents: v } : { tag: 'LogText', contents: String(v) }
  }
  return { tag: 'LogI18n', contents: [key, vars] }
}

/* True when the entire body is one i18n token, with or without parameters.
 * Same rule as i18n.ts's isWholeI18nToken, which this intentionally mirrors. */
function isWholeI18nToken(input: string): boolean {
  if (!input.startsWith('$')) return false
  const spaceIndex = input.indexOf(' ')
  if (spaceIndex === -1) return true
  const rest = input.substring(spaceIndex + 1).trim()
  if (!rest) return false
  const value = '(?:"(?:[^"\\\\]|\\\\.)*"|\\S+)'
  return new RegExp(`^[A-Za-z0-9_]+=[is]:${value}(\\s+[A-Za-z0-9_]+=[is]:${value})*$`).test(rest)
}

function parseBraceRef(fragment: string, locations: LegacyLocationLookup): LogPart | null {
  let m = fragment.match(CARD)
  if (m) return ref('RefCard', m[1], { cardCode: m[2] ?? null, cardId: m[3] ?? null })

  m = fragment.match(INVESTIGATOR)
  if (m) return ref('RefInvestigator', m[1], { entityId: m[2] ?? null, cardCode: m[2] ?? null })

  m = fragment.match(ENEMY)
  if (m) return ref('RefEnemy', m[1], { cardCode: m[3] ?? null })

  m = fragment.match(LOCATION_WITH_CODE)
  if (m) {
    const id = m[2] ?? ''
    /* The old renderer reached into game.locations from inside its render
     * function to decide whether to draw the back. Resolve it here instead; if
     * the location is gone, fall back to the printed code face-up. */
    const live = locations[id.replace(/^"|"$/g, '')]
    return ref('RefLocation', m[1], {
      entityId: id || null,
      cardCode: live?.cardCode ?? m[3] ?? null,
      faceDown: live ? !live.revealed : false,
    })
  }

  m = fragment.match(LOCATION)
  if (m) return ref('RefLocation', m[1], { entityId: m[2] ?? null })

  m = fragment.match(TOKEN)
  if (m) return { tag: 'LogToken', contents: m[1] ?? '' }

  return null
}

/* Split a plain text run on bare `$some.key` tokens, as handleEmbeddedI18n did. */
function parseTextRun(text: string): LogPart[] {
  if (!text) return []
  return text
    .split(I18N_TOKEN)
    .filter((piece) => piece !== '')
    .map((piece) =>
      piece.startsWith('$')
        ? ({ tag: 'LogI18n', contents: [piece.slice(1), {}] } as LogPart)
        : ({ tag: 'LogText', contents: piece } as LogPart),
    )
}

export function parseLegacyLogBody(body: string, locations: LegacyLocationLookup = {}): LogPart[] {
  /* Rows written before the custom-token formatting fix carry the Haskell
   * constructor and an extra pair of quotes. Normalising here keeps those rows
   * readable; the structured format cannot represent the mistake. */
  const normalized = body.replace(/\{token:"CustomToken "([^"]+)""\}/g, '{token:"$1"}')

  const trimmed = normalized.trim()
  if (isWholeI18nToken(trimmed)) return [i18nPart(trimmed)]

  const parts: LogPart[] = []
  for (const fragment of normalized.split(REF_SPLIT)) {
    if (fragment === '') continue
    if (fragment.startsWith('{') && fragment.endsWith('}')) {
      const parsed = parseBraceRef(fragment, locations)
      /* An unrecognised brace fragment is shown verbatim rather than dropped --
       * it is someone's log and losing it silently would be worse than ugly. */
      parts.push(parsed ?? { tag: 'LogText', contents: fragment })
    } else {
      parts.push(...parseTextRun(fragment))
    }
  }
  return parts
}

/* A legacy row as an entry. Flat by definition: the old format had no nesting,
 * no cause and no audience, so these are the honest defaults rather than
 * guesses. Seq is the row's position in history. */
export function legacyLogEntry(
  body: string,
  seq: number,
  locations: LegacyLocationLookup = {},
  step: number | null = null,
): LogEntry {
  return {
    seq,
    step,
    kind: 'Mechanic',
    body: parseLegacyLogBody(body, locations),
    source: null,
    children: [],
    audience: { tag: 'Everyone' },
    context: { round: null, phase: null, turn: null },
    tone: null,
    group: null,
  }
}

/* Normalise a run of rows into entries, parsing the legacy ones exactly once.
 *
 * Legacy rows keep seq 0: the old format had no sequence number and inventing
 * one would be a lie the renderer might come to rely on. They are strictly
 * older than any structured entry in the same game, so the list keys on
 * position for them, which is stable within a page.
 */
export function logRowsToEntries(
  rows: readonly LogRow[],
  locations: LegacyLocationLookup = {},
): LogEntry[] {
  return rows.map((row) =>
    row.tag === 'LogRowStructured'
      ? row.contents
      : legacyLogEntry(row.contents[0], 0, locations, row.contents[1]),
  )
}

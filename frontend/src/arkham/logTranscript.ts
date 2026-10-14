import type { useI18n } from 'vue-i18n'
import { chaosTokenKey } from '@/arkham/types/ChaosToken'
import { formatKey, logKeyTitle } from '@/arkham/types/Log'
import { splitRunes } from '@/arkham/helpers'
import {
  groupLogEntries,
  type LogEntry,
  type LogItem,
  type LogKind,
  type LogPart,
} from '@/arkham/types/GameLog'
import { logRefName, type DbCards } from '@/arkham/logRefName'

/* The game log as plain text, for the export dialog.
 *
 * Rendered HERE and not on the server, which is the whole reason this file
 * exists: every sentence a structured entry carries is an i18n key, and the
 * locales live in the frontend. The server's own text rendering
 * (`logEntryToLines`, served as `format=trace`) prints `$log.scenarioBegins`
 * for the same entry, which is right for a trace and wrong for a transcript.
 *
 * It renders what the panel shows -- the same group blocks, the same resolved
 * card names, the same audience filter -- so a reader can recognise one in the
 * other. What it cannot carry is anything that is only a picture: a chaos token
 * is its name, a skill icon is its name, a rune is its letter.
 */

export type TranslateFn = ReturnType<typeof useI18n>['t']

export interface TranscriptContext {
  t: TranslateFn
  dbCards: DbCards
  /* This client's seat. An entry addressed to one player is dropped from
     everyone else's transcript, as it is from their panel. */
  playerId?: string | null
}

/* A locale lookup that admits when there is no message: vue-i18n hands back the
   key path, so a missing `log.token.*` would otherwise print as a key. Same
   shape as logKeyTitle's fallback in types/Log.ts. */
function translateOr(t: TranslateFn, key: string, fallback: string): string {
  const translated = t(key)
  return translated === key ? fallback : translated
}

/* A rune is a glyph in the panel and has no word to print, so it keeps its
   letter and the locale decides how to frame it. */
function runeText(t: TranslateFn, rune: string): string {
  const framed = t('log.rune', { rune })
  return framed === 'log.rune' ? `[${rune}]` : framed
}

/* Rune markers inside a card name or a text run become their letters rather
   than the raw `[tdc_rune_m]` the markup uses. */
function withRunes(t: TranslateFn, body: string): string {
  return splitRunes(body)
    .map((seg) => (seg.rune ? runeText(t, seg.rune) : seg.text))
    .join('')
}

function titleCase(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1)
}

export function renderLogPart(part: LogPart, ctx: TranscriptContext): string {
  const { t } = ctx
  switch (part.tag) {
    case 'LogText':
      return withRunes(t, part.contents)
    case 'LogI18n': {
      const [key, vars] = part.contents
      const named: Record<string, unknown> = {}
      for (const [name, value] of Object.entries(vars)) {
        named[name] = renderLogPart(value, ctx)
      }
      /* The count drives pluralization, as it does in LogPart.vue: vue-i18n
         needs the number as the plural choice, or a message written with `|`
         branches never picks one. */
      const count = vars['count']
      if (count?.tag === 'LogNumber') return t(key, named, { plural: count.contents })
      return Object.keys(named).length > 0 ? t(key, named) : t(key)
    }
    case 'LogRefPart':
      return withRunes(t, logRefName(part.contents, ctx.dbCards))
    case 'LogNumber':
      return String(part.contents)
    case 'LogDelta':
      return `${part.contents < 0 ? '-' : '+'}${Math.abs(part.contents)}`
    case 'LogToken': {
      const key = chaosTokenKey(part.contents)
      return `[${translateOr(t, `log.token.${key}`, titleCase(key))}]`
    }
    case 'LogIcon':
      return `[${translateOr(t, `log.icon.${part.contents}`, titleCase(part.contents))}]`
    case 'LogCampaignKey':
      return logKeyTitle(formatKey(part.contents), t)
    case 'LogList': {
      const items = part.contents.map((item) => renderLogPart(item, ctx))
      return items.reduce((acc, item, i) => {
        if (i === 0) return item
        const separator = i === items.length - 1 ? t('log.listLast') : t('log.listSeparator')
        return acc + separator + item
      }, '')
    }
  }
}

/* An entry's own sentence, with its cause in parentheses.
 *
 * The cause is carried as data (`source`) rather than written into the sentence,
 * and the panel draws it as a chip beside the line; parentheses are the plain-text
 * equivalent. No label in front of it: "(Ghoul Priest)" needs no translating. */
function entryText(entry: LogEntry, ctx: TranscriptContext): string {
  const body = entry.body.map((part) => renderLogPart(part, ctx)).join('')
  if (!entry.source) return body
  return `${body} (${withRunes(ctx.t, logRefName(entry.source, ctx.dbCards))})`
}

const INDENT = '  '

/* A heading starts a section, so it gets air in front of it -- the panel
   separates them with a band, which text cannot do. Not between consecutive
   headings, though: "Round 2", "Mythos" and "Daisy's turn" arrive in a run and
   belong together, and a blank line between each turns the whole transcript
   into a column of couplets. */
function pushEntry(
  entry: LogEntry,
  depth: number,
  ctx: TranscriptContext,
  out: string[],
  state: { lastWasHeading: boolean },
): void {
  const heading = entry.kind === 'Structure' && depth === 0
  if (heading && out.length > 0 && !state.lastWasHeading) out.push('')
  out.push(INDENT.repeat(depth) + entryText(entry, ctx))
  state.lastWasHeading = heading
  for (const child of entry.children) pushEntry(child, depth + 1, ctx, out, state)
}

function pushItem(
  item: LogItem,
  depth: number,
  ctx: TranscriptContext,
  out: string[],
  state: { lastWasHeading: boolean },
): void {
  if (item.kind === 'entry') {
    pushEntry(item.entry, depth, ctx, out, state)
    return
  }
  /* A block: its header names it, its members and its outcome sit under it. A
     block with no header is one whose header was undone away (see
     dissolveBarless), so its members keep the outer depth. */
  const inner = item.header ? depth + 1 : depth
  if (item.header) pushEntry(item.header, depth, ctx, out, state)
  for (const member of item.members) pushItem(member, inner, ctx, out, state)
  if (item.summary) pushEntry(item.summary, inner, ctx, out, state)
}

/** The transcript as lines, blocks indented the way the panel nests them. */
export function logTranscriptLines(
  entries: readonly LogEntry[],
  ctx: TranscriptContext,
): string[] {
  const visible = entries.filter(
    (e) => e.audience.tag === 'Everyone' || e.audience.contents === ctx.playerId,
  )
  const out: string[] = []
  const state = { lastWasHeading: true }
  for (const item of groupLogEntries(visible)) pushItem(item, 0, ctx, out, state)
  return out
}

export function logTranscript(entries: readonly LogEntry[], ctx: TranscriptContext): string {
  return logTranscriptLines(entries, ctx).join('\n') + '\n'
}

/* The server's own text rendering, mirrored.
 *
 * `format=trace` is produced by `logEntryToLines` in Arkham.Log.Entry, which
 * this follows line for line -- kind tag, two spaces per depth, `$key(var=…)`
 * for an i18n part, ` <- ` for the cause. It exists so the export dialog can
 * SHOW that format instead of describing it; nothing renders a real trace here,
 * the server streams that.
 *
 * If the Haskell changes, this is the copy that drifts. It is a preview, so the
 * cost of drift is a slightly wrong sample -- but keep them together anyway.
 */
function tracePart(part: LogPart): string {
  switch (part.tag) {
    case 'LogText':
      return part.contents
    case 'LogI18n': {
      const [key, vars] = part.contents
      const names = Object.keys(vars).sort()
      if (names.length === 0) return `$${key}`
      /* Sorted: the server holds these in a Map, and Map.toList is ordered by
         key, so an unsorted object would not match what it prints. */
      const shown = names.map((name) => `${name}=${tracePart(vars[name]!)}`)
      return `$${key}(${shown.join(', ')})`
    }
    case 'LogRefPart':
      /* The name the SERVER sent, not the card database's: a trace is what it
         wrote, which is the point of having the format at all. */
      return part.contents.name
    case 'LogNumber':
      return String(part.contents)
    case 'LogDelta':
      return `${part.contents < 0 ? '' : '+'}${part.contents}`
    case 'LogToken':
    case 'LogIcon':
      return `[${part.contents}]`
    case 'LogCampaignKey':
      /* Approximated: the server prints aeson's own Show of the key, which has
         no equivalent here. */
      return JSON.stringify(part.contents)
    case 'LogList':
      return part.contents.map(tracePart).join(', ')
  }
}

const TRACE_KIND: Record<LogKind, string> = {
  Structure: 'structure',
  Action: 'action',
  Mechanic: 'mechanic',
  Test: 'test',
  Narrative: 'narrative',
  Record: 'record',
  Notice: 'notice',
  Problem: 'problem',
  Chat: 'chat',
}

export function logTraceLines(entries: readonly LogEntry[]): string[] {
  const out: string[] = []
  const push = (entry: LogEntry, depth: number) => {
    const body = entry.body.map(tracePart).join('')
    const cause = entry.source ? ` <- ${entry.source.name}` : ''
    const audience = entry.audience.tag === 'Everyone' ? '' : ` (only ${entry.audience.contents})`
    out.push(`${INDENT.repeat(depth)}[${TRACE_KIND[entry.kind]}] ${body}${cause}${audience}`)
    for (const child of entry.children) push(child, depth + 1)
  }
  for (const entry of entries) push(entry, 0)
  return out
}

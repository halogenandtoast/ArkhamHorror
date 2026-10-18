/* The text a custom card carries, and the keys its own labels name.
 *
 * A card's prompts have to read as words in a game whose players do not own the
 * set, so the text travels in the def -- `meta._locale`, a map of language to
 * that card's own keys -- the same way its set's tokens and keywords do. The
 * engine turns a label written `$.again` into `$custom.<card code>.again`, and
 * this is what puts something at that address.
 */
import { escapeIconPlaceholders } from '@/locales/messages'
import { mergeMessages, messagesFor } from '@/locales/i18n'
import type { CardDef } from '@/arkham/types/CardDef'

export const LOCALE_META_KEY = '_locale'
export const TEXT_SCOPE = 'custom'

/** The language a set is written in first, and the one the builder previews. */
export const PRIMARY_LANGUAGE = 'en'

/* The languages a set can be written in: the ones a player can pick, which is
 * wider than the ones the app's own messages are bundled in. A card's words are
 * its author's to write, and a player reading in Polish should find them. */
export const TEXT_LANGUAGES: [string, string][] = [
  ['en', 'English'],
  ['de', 'Deutsch/German'],
  ['es', 'Español/Spanish'],
  ['fr', 'Français/French'],
  ['it', 'Italiano/Italian'],
  ['ko', '한국어/Korean'],
  ['pl', 'Polski/Polish'],
  ['po', 'Português/Portuguese'],
  ['ru', 'Русский/Russian'],
  ['uk', 'українська/Ukrainian'],
  ['zh-cn', '简体中文/Simplified Chinese'],
  ['zh', '中文/Chinese'],
]

/* ---------------------------------------------------------- shared text ---

   The words the app already has.

   A prompt like "Add {name} to hand" is written once in `label.json` and said
   by a hundred printed cards; a custom card asking for it by name gets every
   language it has been translated into, which no amount of writing its own
   text would give it. `$label.addToHand name=s:"Impaling Swords"` is the whole
   of it: the key, and values for whatever the sentence leaves blank.
*/

/** Where the shared prompts live. Engine labels have used this namespace all along. */
export const SHARED_TEXT_ROOT = 'label'

/* A key the card did not write: `$label.addToHand`, as against `$.again` for
 * one it did. A bare `$name` with no dot is a binding, not a key. */
export const isSharedTextKey = (value: unknown): value is string =>
  typeof value === 'string' && value.startsWith('$') && !value.startsWith('$.') && value.includes('.')

export type SharedText = { key: string; text: string; params: string[]; plural: boolean }

/* What a message leaves blank. vue-i18n's literal form -- `{'{'}` -- is an
 * escaped brace rather than a parameter, so it is not one of these. */
const PLACEHOLDER = /(?<!')\{([A-Za-z_][A-Za-z0-9_]*)\}/g

export function sharedTextCatalogue(language = PRIMARY_LANGUAGE): SharedText[] {
  const root = messagesFor(language)[SHARED_TEXT_ROOT]
  const found: SharedText[] = []
  const walk = (value: unknown, prefix: string) => {
    if (typeof value === 'string') {
      const params = [...new Set([...value.matchAll(PLACEHOLDER)].map((m) => m[1]))]
      /* A message written in more than one wording picks between them by the
       * number in `count` -- which is what `countVar` sends -- so it needs one
       * whether or not any branch prints it. */
      const plural = value.includes('|')
      if (plural && !params.includes('count')) params.push('count')
      found.push({ key: `${SHARED_TEXT_ROOT}.${prefix}`, text: value, params, plural })
    } else if (value && typeof value === 'object') {
      for (const [k, v] of Object.entries(value)) walk(v, prefix ? `${prefix}.${k}` : k)
    }
  }
  walk(root, '')
  return found
}

/* A label and the values it carries: `$label.addToHand name=s:"X"`. The engine
 * passes it through untouched and the client reads it with `parseInput`, so
 * this is that format written rather than read. */
export function parseSharedLabel(label: string): { key: string; params: Record<string, string> } {
  const trimmed = label.trim().replace(/^\$/, '')
  const space = trimmed.indexOf(' ')
  if (space === -1) return { key: trimmed, params: {} }
  const params: Record<string, string> = {}
  const rest = trimmed.slice(space + 1)
  for (const [, name, kind, value] of rest.matchAll(/([^=\s]+)=([is]):("(?:[^"\\]|\\.)*"|\S+)/g)) {
    params[name] = kind === 's' ? value.replace(/^"|"$/g, '') : value
  }
  return { key: trimmed.slice(0, space), params }
}

/* The values as the client reads them rather than as they are typed.
 *
 * `count=i:3` arrives as a number at run time (`parseInput` parses it), and it
 * has to be one here too: a message written in branches picks between them on
 * a number, and a string that looks like one silently takes the first branch.
 */
export function readableParams(params: Record<string, string>): Record<string, unknown> {
  const out: Record<string, unknown> = {}
  for (const [name, value] of Object.entries(params)) {
    out[name] = /^-?\d+$/.test(value) ? Number(value) : value
  }
  return out
}

/** A value that is nothing but a name in scope, which the engine fills in. */
export const isBindingValue = (value: string) => /^\$[A-Za-z_][A-Za-z0-9_]*$/.test(value)

/* `numeric` names the blanks that must travel as numbers rather than as text:
 * the ones a plural message counts on. A binding is written bare after `i:` so
 * that what the engine puts in its place is a number and not a quoted one. */
export function formatSharedLabel(
  key: string,
  params: Record<string, string>,
  numeric: string[] = [],
): string {
  const written = Object.entries(params)
    .filter(([, value]) => value !== '' && value !== undefined)
    .map(([name, value]) =>
      numeric.includes(name) && isBindingValue(value)
        ? `${name}=i:${value}`
        :
      /* A number goes as a number, which is what picks the wording of a
       * message written with plural branches -- `countVar` on the engine side
       * writes the same thing. Everything else is quoted, escaping the two
       * characters the client's unquoting reads: the quote and the backslash. */
      /^-?\d+$/.test(value)
        ? `${name}=i:${value}`
        : `${name}=s:"${value.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`,
    )
  return `$${[key, ...written].join(' ')}`
}

/* How a field deep in the editor reaches the text of the card being edited,
 * without every editor between here and there carrying it as a prop. */
export const CARD_TEXT = Symbol('customCardText')

export type CardTextAccess = {
  read: (key: string) => string
  write: (key: string, value: string) => void
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
type Def = CardDef | Record<string, any>

const localeBlock = (def: Def): Record<string, Record<string, string>> => {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const block = (def as any)?.meta?.[LOCALE_META_KEY]
  return block && typeof block === 'object' ? block : {}
}

/** What this card says in one language, by its own keys. */
export function cardText(def: Def, language = PRIMARY_LANGUAGE): Record<string, string> {
  const text = localeBlock(def)[language]
  return text && typeof text === 'object' ? text : {}
}

/* A label the card wrote against itself. `$.again` is the whole of what an
 * author types; the rest is the card's identity, which it already has. */
export const isOwnTextKey = (value: unknown): value is string =>
  typeof value === 'string' && value.startsWith('$.') && value.length > 2

export const ownTextKey = (label: string) => label.slice(2)

/* Every key a def names, in the order they are met, so the editor can offer
 * exactly the text this card needs and no more. A key written twice is one
 * key: the same prompt said in two places is the same words. */
export function textKeysUsed(def: Def): string[] {
  const found: string[] = []
  const walk = (value: unknown) => {
    if (isOwnTextKey(value)) {
      const key = ownTextKey(value)
      if (!found.includes(key)) found.push(key)
    } else if (Array.isArray(value)) value.forEach(walk)
    else if (value && typeof value === 'object') Object.values(value).forEach(walk)
  }
  walk(def)
  return found
}

/* The address the engine gives this card's text. The code carries the leading
 * `c` the wire form has, because that is what the engine writes into the label
 * it sends. */
export const textScopeOf = (cardCode: string) =>
  `${TEXT_SCOPE}.${cardCode.startsWith('c') ? cardCode : `c${cardCode}`}`

/* Hand a card's text to vue-i18n, under the card's own scope.
 *
 * Merged rather than set: cards arrive in batches, a game may hold several
 * sets, and a language the app has not loaded gets one of its own rather than
 * nothing. Icons are escaped the way the bundled messages are, so `{skull}`
 * means the same thing written on a custom card as on a printed one. */
export function publishCardText(defs: Def[]) {
  const byLanguage: Record<string, Record<string, unknown>> = {}
  for (const def of defs) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const code = (def as any)?.cardCode
    if (typeof code !== 'string' || !code) continue
    const scope = textScopeOf(code).split('.')[1]
    for (const [language, text] of Object.entries(localeBlock(def))) {
      if (!text || typeof text !== 'object') continue
      const escaped: Record<string, string> = {}
      for (const [key, value] of Object.entries(text)) {
        if (typeof value === 'string') escaped[key] = escapeIconPlaceholders(value)
      }
      const bucket = (byLanguage[language] ??= {})
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const scoped = ((bucket as any)[TEXT_SCOPE] ??= {})
      scoped[scope] = { ...(scoped[scope] ?? {}), ...escaped }
    }
  }
  for (const [language, messages] of Object.entries(byLanguage)) mergeMessages(language, messages)
}

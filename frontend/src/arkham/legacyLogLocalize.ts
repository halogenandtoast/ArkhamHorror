import { messagesFor } from '@/locales/i18n'
import type { LogPart, LogRef } from '@/arkham/types/GameLog'

/* Localizing the English sentences of legacy log rows.
 *
 * A legacy row is English glue between refs: `{investigator:…} played {card:…}`.
 * The refs already render localized; the glue did not, so In games with different word orders, 
 * it is read incorrectly.
 * Each sentence the engine ever sent this way is a closed, known template, 
 * so it is matched here and rewritten into the same
 * `LogI18n` part a structured entry carries -- reusing the structured key where
 * one says the same thing (playsCard, drawsChaosTokens, discoveredClues).
 *
 * Runs at ingest with parseLegacyLogBody. Anything unmatched is left as it was.
 */

const REF = '\u0001'
/* One or more refs joined the way toSentence joins them: "a", "a and b",
 * "a, b, and c". */
const LIST = `${REF}(?:(?:, and |, | and )${REF})*`

type Vars = Record<string, LogPart>
type Build = (groups: string[], refs: LogPart[][]) => LogPart[] | null

interface Template {
  re: RegExp
  build: Build
}

const num = (n: string): LogPart => ({ tag: 'LogNumber', contents: parseInt(n, 10) })
const list = (items: LogPart[]): LogPart => (items.length === 1 ? items[0]! : { tag: 'LogList', contents: items })
const i18n = (key: string, vars: Vars = {}): LogPart => ({ tag: 'LogI18n', contents: [key, vars] })
const one = (key: string, vars: Vars): LogPart[] => [i18n(key, vars)]

/* Token labels as VoiceOfRa printed them ("Elder Thing", "-2"), back to the
 * faces a `{token:…}` ref carries. */
const TOKEN_LABELS: Record<string, string> = {
  '+1': 'PlusOne', '0': 'Zero', '-1': 'MinusOne', '-2': 'MinusTwo', '-3': 'MinusThree',
  '-4': 'MinusFour', '-5': 'MinusFive', '-6': 'MinusSix', '-7': 'MinusSeven', '-8': 'MinusEight',
  Skull: 'Skull', Cultist: 'Cultist', Tablet: 'Tablet', 'Elder Thing': 'ElderThing',
  'Auto Fail': 'AutoFail', 'Elder Sign': 'ElderSign', Curse: 'CurseToken', Bless: 'BlessToken',
  Frost: 'FrostToken', Blood: 'BloodToken',
}

const INTERVIEWED: Record<string, string> = {
  constance: '03076a', jordan: '03077', haruko: '03078', sebastien: '03079', ashleigh: '03080',
}

const TEMPLATES: Template[] = [
  {
    re: new RegExp(`^(${REF}) played (${REF})$`),
    build: (_, [inv, card]) => one('log.playsCard', { investigator: inv![0]!, card: card![0]! }),
  },
  {
    re: new RegExp(`^(${REF}) draws (${LIST}) chaos tokens?$`),
    build: (_, [inv, tokens]) =>
      one('log.drawsChaosTokens', {
        investigator: inv![0]!,
        tokens: list(tokens!),
        count: { tag: 'LogNumber', contents: tokens!.length },
      }),
  },
  {
    re: new RegExp(`^(${REF}) discovered (\\d+) clues?$`),
    build: ([, , n], [inv]) => one('log.discoveredClues', { investigator: inv![0]!, count: num(n!) }),
  },
  {
    re: new RegExp(`^(${REF}) must discard down to (\\d+) cards$`),
    build: ([, , n], [inv]) => one('log.legacy.discardDownTo', { investigator: inv![0]!, count: num(n!) }),
  },
  {
    re: new RegExp(`^(${REF}) removed all copies of (${REF}) from the game$`),
    build: (_, [source, card]) => one('log.legacy.removedAllCopies', { source: source![0]!, card: card![0]! }),
  },
  {
    re: new RegExp(`^(${REF}) reveals (${LIST})$`),
    build: (_, [inv, cards]) => one('log.legacy.reveals', { investigator: inv![0]!, cards: list(cards!) }),
  },
  {
    re: new RegExp(`^(${REF}) devours (${REF})$`),
    build: (_, [inv, card]) => one('log.legacy.devours', { investigator: inv![0]!, card: card![0]! }),
  },
  {
    re: new RegExp(`^(${REF}) is "banished"$`),
    build: (_, [card]) => one('log.legacy.banished', { card: card![0]! }),
  },
  {
    re: new RegExp(`^The coin lands on (heads|tails)\\. Treating token as (${REF})$`),
    build: ([, side], [, token]) => one(`log.legacy.coin.${side}`, { token: token![0]! }),
  },
  {
    /* VoiceOfRa wrote the tokens as quoted labels, not refs. */
    re: new RegExp(`^(${REF}) drew ((?:"[^"]+"(?:, and |, | and )?)+)$`),
    build: ([, , labels], [card]) => {
      const faces = [...labels!.matchAll(/"([^"]+)"/g)].map((m) => TOKEN_LABELS[m[1]!])
      if (faces.some((f) => !f)) return null
      const tokens = faces.map((f): LogPart => ({ tag: 'LogToken', contents: f! }))
      return one('log.legacy.drew', { investigator: card![0]!, card: list(tokens) })
    },
  },
  {
    re: new RegExp(`^(${REF}) drew (${LIST})$`),
    build: (_, [inv, cards]) => one('log.legacy.drew', { investigator: inv![0]!, card: list(cards!) }),
  },
  {
    re: /^Record "(.*)"$/s,
    build: ([, body]) => recordParts(body!),
  },
  {
    /* The Last King's interviews have no campaign-log message of their own;
     * the key names the VIP, so name their card. */
    re: /^(Remember|Forgot) "interviewed (constance|jordan|haruko|sebastien|ashleigh)"$/,
    build: ([, verb, who]) => [
      i18n(verb === 'Remember' ? 'log.legacy.remembered' : 'log.legacy.forgot'),
      { tag: 'LogText', contents: ' ' },
      i18n('log.legacy.interviewed', {
        name: nameRef(who!, INTERVIEWED[who!]!),
      }),
    ],
  },
  {
    re: new RegExp(`^(Remember|Forgot) "((?:[^${REF}]|${REF})*)"$`),
    build: ([, verb, body], refs) => rememberParts(verb as 'Remember' | 'Forgot', body!, refs.flat()),
  },
]

const camel = (words: string[]) =>
  words
    .filter(Boolean)
    .map((w, i) => (i === 0 ? w.toLowerCase() : w.charAt(0).toUpperCase() + w.slice(1).toLowerCase()))
    .join('')

/* Haskell constructor names render through `format` as lower-case words
 * ("TheForgottenAgeKey TheInvestigatorsRescuedAlejandro" -> "the forgotten age
 * key  the investigators rescued alejandro"), so a recorded key is rebuilt by
 * camel-casing those words, the same names formatKey derives from the JSON.
 * Upper-case runs lose their shape ("VIPs" -> "v i ps") but camel-case back to
 * the same "vIPs". */

function hasKey(path: string): boolean {
  let node: unknown = messagesFor('en')
  for (const seg of path.split('.')) {
    if (!node || typeof node !== 'object') return false
    node = (node as Record<string, unknown>)[seg]
  }
  return typeof node === 'string'
}

function recordParts(body: string): LogPart[] | null {
  const at = body.indexOf(' key  ')
  if (at < 0) return null
  const key = camel(body.slice(at + 6).replace(/'/g, '').split(' '))
  /* A name may precede the campaign ("Jordan Perry: Dignified Financier the
   * path to carcosa key  …"), so try each word as the campaign's first. */
  const words = body.slice(0, at).trim().split(' ')
  for (let i = 0; i < words.length; i++) {
    const campaign = camel(words.slice(i))
    const path = `${campaign}.key.${key}`
    if (!hasKey(path)) continue
    const name = words.slice(0, i).join(' ')
    const parts: LogPart[] = [i18n('log.legacy.recorded'), { tag: 'LogText', contents: ' ' }]
    if (name) {
      parts.push(nameRef(name), { tag: 'LogText', contents: ' — ' })
    }
    parts.push(i18n(path))
    return parts
  }
  return null
}

/* A recorded name is the card's full title ("Constance Dumaine: Sociable
 * Hostess"); as a ref it localizes through the card database like any other. */
function nameRef(title: string, cardCode: string | null = null): LogPart {
  const ref: LogRef = { kind: 'RefCard', name: title, cardCode, cardId: null, entityId: null, faceDown: false }
  return { tag: 'LogRefPart', contents: ref }
}

/* Every `<…>.remembered.<key>` message in the English locale, by key. A legacy
 * row does not say which scenario it came from, so the key alone finds it. */
let rememberedIndex: Map<string, string> | null = null

function indexRemembered(): Map<string, string> {
  if (rememberedIndex && rememberedIndex.size > 0) return rememberedIndex
  const index = new Map<string, string>()
  const walk = (node: unknown, path: string[]) => {
    if (!node || typeof node !== 'object') return
    for (const [k, v] of Object.entries(node as Record<string, unknown>)) {
      if (path[path.length - 1] === 'remembered' && typeof v === 'string') {
        if (!index.has(k)) index.set(k, [...path, k].join('.'))
      } else {
        walk(v, [...path, k])
      }
    }
  }
  walk(messagesFor('en'), [])
  rememberedIndex = index
  return index
}

function rememberParts(verb: 'Remember' | 'Forgot', body: string, refs: LogPart[]): LogPart[] | null {
  const words = body.replace(new RegExp(REF, 'g'), ' ').replace(/'/g, '').trim().split(/\s+/)
  const index = indexRemembered()
  /* "{investigator} is Ichtaca's Destination" is remembered as
   * ichtacasDestination: drop leading glue words until a key matches. */
  for (let i = 0; i < words.length; i++) {
    const path = index.get(camel(words.slice(i)))
    if (!path) continue
    const vars: Vars = refs[0] ? { name: refs[0] } : {}
    return [
      i18n(verb === 'Remember' ? 'log.legacy.remembered' : 'log.legacy.forgot'),
      { tag: 'LogText', contents: ' ' },
      i18n(path, vars),
    ]
  }
  return null
}

/* Rewrite a legacy row's parts if they are one of the known sentences. */
export function localizeLegacyParts(parts: LogPart[]): LogPart[] {
  const refs = parts.filter((p) => p.tag !== 'LogText')
  if (parts.some((p) => p.tag === 'LogI18n')) return parts
  const signature = parts.map((p) => (p.tag === 'LogText' ? p.contents : REF)).join('')

  for (const { re, build } of TEMPLATES) {
    const m = signature.match(re)
    if (!m) continue
    /* Hand each capture group the refs it covers, in order. */
    let at = 0
    const groups = [...m] as string[]
    const refGroups = groups.slice(1).map((g) => {
      const n = (g ?? '').split(REF).length - 1
      const slice = refs.slice(at, at + n)
      at += n
      return slice
    })
    const built = build(groups, refGroups)
    if (built) return built
  }
  return parts
}

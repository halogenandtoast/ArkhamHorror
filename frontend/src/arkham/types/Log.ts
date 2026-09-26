import * as JsonDecoder from 'ts.data.json';
import { homebrewCampaignScope } from '@/arkham/homebrewData';

type PartnerStatus = 'Eliminated' | 'Resolute' | 'Mia' | 'Safe' | 'Victim' | 'CannotTake' | 'TheEntity'

interface Partner {
  damage: number;
  horror: number;
  status: PartnerStatus;
}

export interface SomeRecordable {
  recordType: string;
  recordVal: unknown;
}

const someRecordableDecoder = JsonDecoder.object<SomeRecordable>({
  recordType: JsonDecoder.string(),
  recordVal: JsonDecoder.succeed()
}, 'SomeRecordable')

/* The `Recorded` inside, with the recordable type kept alongside it.
 *
 * Everything that renders an entry reads `tag`, `circled` and `contents` off the
 * one object, so the wrapper is flattened away -- but `recordType` has to survive
 * it. It is the only thing that says whether an entry is a card code, and dropping
 * it left the log treating a recorded trait as a card it could not find and
 * printing "unknown".
 */
const flattenRecordable = (res: SomeRecordable): unknown =>
  isRecord(res.recordVal)
    ? { ...res.recordVal, recordType: res.recordType }
    : { contents: res.recordVal, recordType: res.recordType }

export type LogKey = {
  tag: string
  contents?: string | { tag: string; contents?: string }
  actualTag?: string
  name?: string
}

export type CampaignOptionTag = { tag: string }

export type LogContents = {
  recorded: LogKey[];
  recordedSets: Record<string, unknown[]>;
  recordedCounts: [LogKey, number][];
  partners: Record<string, Partner>;
  options: CampaignOptionTag[];
}

export const partnerStatusDecoder = JsonDecoder.oneOf<PartnerStatus>([
  JsonDecoder.literal('Eliminated'),
  JsonDecoder.literal('Resolute'),
  JsonDecoder.literal('Mia'),
  JsonDecoder.literal('Safe'),
  JsonDecoder.literal('Victim'),
  JsonDecoder.literal('CannotTake'),
  JsonDecoder.literal('TheEntity'),
], 'PartnerStatus');

export const partnerDecoder = JsonDecoder.object<Partner>({
  damage: JsonDecoder.number(),
  horror: JsonDecoder.number(),
  status: partnerStatusDecoder,
}, 'Partner');

const isRecord = (v: unknown): v is Record<string, unknown> =>
  typeof v === "object" && v !== null

export const logKeyDecoder: JsonDecoder.Decoder<LogKey> = JsonDecoder.succeed().flatMap((value: unknown) => {
  if (!isRecord(value) || typeof value.tag !== 'string') return JsonDecoder.fail('Expected LogKey')
  const key: LogKey = { tag: value.tag }
  if (typeof value.contents === 'string') {
    key.contents = value.contents.replace(/'/g, '')
  } else if (isRecord(value.contents) && typeof value.contents.tag === 'string') {
    key.contents = { tag: value.contents.tag }
    if (typeof value.contents.contents === 'string') key.contents.contents = value.contents.contents.replace(/'/g, '')
  }
  if (typeof value.actualTag === 'string') key.actualTag = value.actualTag
  if (typeof value.name === 'string') key.name = value.name
  return JsonDecoder.constant(key)
})

const campaignOptionTagDecoder = JsonDecoder.object<CampaignOptionTag>({
  tag: JsonDecoder.string(),
}, 'CampaignOptionTag');

export const logContentsDecoder = JsonDecoder.object<LogContents>({
  recorded: JsonDecoder.array<LogKey>(logKeyDecoder, 'LogKey[]'),
  recordedSets: JsonDecoder.array<[LogKey, unknown[]]>(JsonDecoder.tuple([logKeyDecoder, JsonDecoder.array(someRecordableDecoder.map(flattenRecordable), 'SomeRecorded[]')], '[string, somerecorded]'), '[string, unknown][]').map<Record<string, unknown[]>>(res => {
    return res.reduce<Record<string, unknown[]>>((acc, [k, v]) => {
      return {[formatKey(k)]: v, ...acc}
    }, {})
  }),
  recordedCounts: JsonDecoder.array<[LogKey, number]>(JsonDecoder.tuple([logKeyDecoder, JsonDecoder.number()], '[LogKey, number]'), '[LogKey, number][]'),
  partners: JsonDecoder.record<Partner>(partnerDecoder, 'Partners'),
  options: JsonDecoder.array<CampaignOptionTag>(campaignOptionTagDecoder, 'CampaignOptionTag[]'),
}, 'LogContents');

export function baseKey(k: string): string {
  return formatKey({ tag: k });
}

function hasContents(key: LogKey): key is LogKey & { contents: string | { tag: string; contents?: string } } {
  return "contents" in key
}

function isTagOnlyContents(x: unknown): x is { tag: string } {
  return isRecord(x) && typeof x.tag === "string" && !("contents" in x)
}

function isNestedContents(x: unknown): x is { tag: string; contents: string } {
  return isRecord(x) && typeof x.tag === "string" && "contents" in x && typeof x.contents === "string"
}

// ":circus-ex-mortis" -> "circusExMortis", or undefined for official campaigns.
export function homebrewScopeFromCampaignId(campaignId: string | undefined): string | undefined {
  if (!campaignId || !campaignId.startsWith(':')) return undefined
  return homebrewCampaignScope(campaignId)
}

/* The human title for a log key path.
 *
 * Every campaign's keys have locale entries, so `t` normally answers. A homebrew
 * key invented by a custom card never will -- nobody can add a locale entry for a
 * name a player made up in the builder -- and vue-i18n answers a miss with the key
 * itself, which is how `thirstForKnowledge.key.traitsLearned` ended up on screen as
 * a title. Its own last segment, spaced out, is the best title available.
 *
 * Deliberately not `toCapitalizedWords` from helpers: that module reaches the
 * settings stores, and a types module should not.
 */
export function logKeyTitle(path: string, t: (key: string) => string): string {
  const translated = t(path)
  if (translated !== path) return translated
  const name = path.split('.').pop() ?? path
  const words = name.match(/[A-Z]?[a-z']+|[A-Z]+(?![a-z])|\d+/g) ?? [name]
  const spaced = words.join(' ').toLowerCase()
  return spaced.charAt(0).toUpperCase() + spaced.slice(1)
}

export function formatKey(key: LogKey, fallbackHomebrewScope?: string): string {
  const format = (str: string) => (str.slice(0, 1).toLowerCase() + str.slice(1)).replace(/'/g, '')

  const prefix = format(key.tag.replace(/Key$/, ""))

  if (!hasContents(key)) {
    return `base.key.${format(key.tag)}`
  }

  if (isNestedContents(key.contents)) {
    const section = format(key.contents.tag)
    const suffix = format(key.contents.contents)
    return `${prefix}.key['[${section}]'].${suffix}`
  }

  if (isTagOnlyContents(key.contents)) {
    return `${prefix}.key.${format(key.contents.tag)}`
  }

  if (typeof key.contents === "string") {
    const [homebrewScope, homebrewKey] = key.contents.split('.', 2)
    if (key.tag === 'HomebrewCampaignLogKey') {
      if (homebrewKey) return `${format(homebrewScope)}.key.${format(homebrewKey)}`
      // Keys recorded before the campaign added its scope prefix carry no scope of
      // their own, so fall back to the campaign whose log we are rendering.
      if (fallbackHomebrewScope) return `${fallbackHomebrewScope}.key.${format(key.contents)}`
    }
    return `${prefix}.key.${format(key.contents)}`
  }

  return `${prefix}.key.unknown`
}

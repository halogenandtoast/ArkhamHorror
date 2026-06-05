import * as JsonDecoder from 'ts.data.json';

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
  recordedSets: JsonDecoder.array<[LogKey, unknown[]]>(JsonDecoder.tuple([logKeyDecoder, JsonDecoder.array(someRecordableDecoder.map((res) => res.recordVal), 'SomeRecorded[]')], '[string, somerecorded]'), '[string, unknown][]').map<Record<string, unknown[]>>(res => {
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

export function formatKey(key: LogKey): string {
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
    return `${prefix}.key.${format(key.contents)}`
  }

  return `${prefix}.key.unknown`
}

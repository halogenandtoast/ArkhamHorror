import * as JsonDecoder from 'ts.data.json';

type PartnerStatus = 'Eliminated' | 'Resolute' | 'Mia' | 'Safe' | 'Victim' | 'CannotTake' | 'TheEntity'

interface Partner {
  damage: number;
  horror: number;
  status: PartnerStatus;
}

export interface SomeRecordable {
  recordType: string;
  recordVal: any; // eslint-disable-line
}

const someRecordableDecoder = JsonDecoder.object<SomeRecordable>({
  recordType: JsonDecoder.string(),
  recordVal: JsonDecoder.succeed()
}, 'SomeRecordable')

export type LogKey= 
  | { tag: string, contents: string }
  | { tag: string, contents: { tag: string, contents: string } }
  | { tag: string, contents: { tag: string } }
  | { tag: string }

export type LogContents = {
  recorded: LogKey[];
  recordedSets: Record<string, any[]>; // eslint-disable-line
  recordedCounts: [LogKey, number][]; // eslint-disable-line
  partners: Record<string, Partner>;
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

export const logKeyDecoder = JsonDecoder.oneOf<LogKey>([
  JsonDecoder.object<LogKey>({
    tag: JsonDecoder.string(),
    contents: JsonDecoder.string().map((str) => str.replace(/'/g, '')),
  }, 'LogKey'),
  JsonDecoder.object<LogKey>({
    tag: JsonDecoder.string(),
    contents: JsonDecoder.object<{ tag: string, contents: string }>({
      tag: JsonDecoder.string(),
      contents: JsonDecoder.string().map((str) => str.replace(/'/g, '')),
    }, 'LogKeyContents'),
  }, 'LogKey'),
  JsonDecoder.object<LogKey>({
    tag: JsonDecoder.string(),
    contents: JsonDecoder.object<{ tag: string }>({
      tag: JsonDecoder.string(),
    }, 'LogKeyContents'),
  }, 'LogKey'),
  JsonDecoder.object<LogKey>({
    tag: JsonDecoder.string(),
  }, 'LogKey'),
], 'LogKey');

export const logContentsDecoder = JsonDecoder.object<LogContents>({
  recorded: JsonDecoder.array<LogKey>(logKeyDecoder, 'LogKey[]'),
  recordedSets: JsonDecoder.array<[LogKey, any[]]>(JsonDecoder.tuple([logKeyDecoder, JsonDecoder.array(someRecordableDecoder.map((res) => res.recordVal), 'SomeRecorded[]')], '[string, somerecorded]'), '[string, any][]').map<Record<string, any>>(res => { // eslint-disable-line
    return res.reduce<Record<string, any>>((acc, [k, v]) => { //eslint-disable-line
      return {[formatKey(k)]: v, ...acc}
    }, {})
  }),
  recordedCounts: JsonDecoder.array<[LogKey, number]>(JsonDecoder.tuple([logKeyDecoder, JsonDecoder.number()], '[LogKey, number]'), '[LogKey, number][]'),
  partners: JsonDecoder.record<Partner>(partnerDecoder, 'Partners'),
}, 'LogContents');

export function baseKey(k: string): string {
  return formatKey({ tag: k });
}

const isRecord = (v: unknown): v is Record<string, unknown> =>
  typeof v === "object" && v !== null

function hasContents(key: LogKey): key is LogKey & { contents: unknown } {
  return isRecord(key) && "contents" in key
}

function isTagOnlyContents(x: unknown): x is { tag: string } {
  return isRecord(x) && typeof x.tag === "string" && !("contents" in x)
}

function isNestedContents(x: unknown): x is { tag: string; contents: string } {
  return isRecord(x) && typeof x.tag === "string" && "contents" in x && typeof (x as any).contents === "string"
}

export function formatKey(key: LogKey): string {
  const format = (str: string) => str.slice(0, 1).toLowerCase() + str.slice(1)

  // remove 'Key' from the end of the tag if it exists
  const prefix = format(key.tag.replace(/Key$/, ""))

  console.log(key, prefix)

  if (!hasContents(key)) {
    return `base.key.${format(key.tag)}`
  }

  // Section/nested form: { tag, contents: { tag, contents } }
  if (isNestedContents(key.contents)) {
    const section = format(key.contents.tag)
    const suffix = format(key.contents.contents)
    return `${prefix}.key['[${section}]'].${suffix}`
  }

  // Tag-only form: { tag, contents: { tag } }
  if (isTagOnlyContents(key.contents)) {
    return `${prefix}.key.${format(key.contents.tag)}`
  }

  // String form: { tag, contents: string }
  if (typeof key.contents === "string") {
    return `${prefix}.key.${format(key.contents)}`
  }

  // Last-resort fallback to help you spot bad shapes in data:
  return `${prefix}.key.unknown`
}

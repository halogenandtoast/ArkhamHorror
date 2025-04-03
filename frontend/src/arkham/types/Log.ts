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

export type LogKey = 
  { tag: string, contents: string } | { tag: string }

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
  partners: JsonDecoder.dictionary<Partner>(partnerDecoder, 'Partners'),
}, 'LogContents');

export function baseKey(k: string): string {
  return formatKey({ tag: k });
}

export function formatKey(key: LogKey): string {
  const format = (str: string) => str.slice(0, 1).toLowerCase() + str.slice(1); 
  if ('contents' in key) {
    // remove 'Key' from the end of the tag if it exists
    const prefix = format(key.tag.replace(/Key$/, ''));
    const suffix = format(key.contents);
    return `${prefix}.key.${suffix}`;
  } else {
    return `base.key.${format(key.tag)}`;
  }
}

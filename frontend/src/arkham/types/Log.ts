import { JsonDecoder } from 'ts.data.json';

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
  recordType: JsonDecoder.string,
  recordVal: JsonDecoder.succeed
}, 'SomeRecordable')

export type LogContents = {
  recorded: string[];
  recordedSets: Record<string, any[]>; // eslint-disable-line
  recordedCounts: [string, number][]; // eslint-disable-line
  partners: Record<string, Partner>;
}

export const partnerStatusDecoder = JsonDecoder.oneOf<PartnerStatus>([
  JsonDecoder.isExactly('Eliminated'),
  JsonDecoder.isExactly('Resolute'),
  JsonDecoder.isExactly('Mia'),
  JsonDecoder.isExactly('Safe'),
  JsonDecoder.isExactly('Victim'),
  JsonDecoder.isExactly('CannotTake'),
  JsonDecoder.isExactly('TheEntity'),
], 'PartnerStatus');

export const partnerDecoder = JsonDecoder.object<Partner>({
  damage: JsonDecoder.number,
  horror: JsonDecoder.number,
  status: partnerStatusDecoder,
}, 'Partner');

export const logContentsDecoder = JsonDecoder.object<LogContents>({
  recorded: JsonDecoder.array<string>(JsonDecoder.string, 'recorded[]'),
  recordedSets: JsonDecoder.array<[string, any[]]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.array(someRecordableDecoder.map((res) => res.recordVal), 'SomeRecorded[]')], '[string, somerecorded]'), '[string, any][]').map<Record<string, any>>(res => { // eslint-disable-line
    return res.reduce<Record<string, any>>((acc, [k, v]) => { //eslint-disable-line
      return {[k]: v, ...acc}
    }, {})
  }),
  recordedCounts: JsonDecoder.array<[string, number]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.number], '[string, number]'), '[string, number][]'),
  partners: JsonDecoder.dictionary<Partner>(partnerDecoder, 'Partners'),
}, 'LogContents');

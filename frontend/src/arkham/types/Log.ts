import { JsonDecoder } from 'ts.data.json';

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
}

export const logContentsDecoder = JsonDecoder.object<LogContents>({
  recorded: JsonDecoder.array<string>(JsonDecoder.string, 'recorded[]'),
  recordedSets: JsonDecoder.array<[string, any[]]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.array(someRecordableDecoder.map((res) => res.recordVal), 'SomeRecorded[]')], '[string, somerecorded]'), '[string, any][]').map<Record<string, any>>(res => { // eslint-disable-line
    return res.reduce<Record<string, any>>((acc, [k, v]) => { //eslint-disable-line
      acc[k] = v
      return acc
    }, {})
  }),
  recordedCounts: JsonDecoder.array<[string, number]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.number], '[string, number]'), '[string, number][]'),
}, 'LogContents');

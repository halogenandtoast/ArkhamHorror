import { JsonDecoder } from 'ts.data.json';
import { Difficulty, difficultyDecoder } from '@/arkham/types/Difficulty';

export type CampaignStep = PrologueStep | ScenarioStep | InterludeStep | UpgradeDeckStep | EpilogueStep | ResupplyPoint

export interface PrologueStep {
  tag: 'PrologueStep';
}

export interface ResupplyPoint {
  tag: 'ResupplyPoint';
}

export interface EpilogueStep {
  tag: 'EpilogueStep';
}

export const prologueStepDecoder = JsonDecoder.object<PrologueStep>(
  {
    tag: JsonDecoder.isExactly('PrologueStep'),
  },
  'PrologueStep',
);

export const resupplyPointStepDecoder = JsonDecoder.object<ResupplyPoint>(
  {
    tag: JsonDecoder.isExactly('ResupplyPoint'),
  },
  'ResupplyPoint',
);

export const epilogueStepDecoder = JsonDecoder.object<EpilogueStep>(
  {
    tag: JsonDecoder.isExactly('EpilogueStep'),
  },
  'EpilogueStep',
);

export interface ScenarioStep {
  tag: 'ScenarioStep';
  contents: string;
}

export const scenarioStepDecoder = JsonDecoder.object<ScenarioStep>(
  {
    tag: JsonDecoder.isExactly('ScenarioStep'),
    contents: JsonDecoder.string
  },
  'ScenarioStep',
);

export interface InterludeStep {
  tag: 'InterludeStep';
}

export const interludeStepDecoder = JsonDecoder.object<InterludeStep>(
  {
    tag: JsonDecoder.isExactly('InterludeStep'),
  },
  'InterludeStep',
);

export interface UpgradeDeckStep {
  tag: 'UpgradeDeckStep';
}

export const upgradeStepDecoder = JsonDecoder.object<UpgradeDeckStep>(
  {
    tag: JsonDecoder.isExactly('UpgradeDeckStep'),
  },
  'UpgradeDeckStep',
);

export const campaignStepDecoder = JsonDecoder.oneOf<CampaignStep>(
  [
    prologueStepDecoder,
    resupplyPointStepDecoder,
    scenarioStepDecoder,
    interludeStepDecoder,
    upgradeStepDecoder,
    epilogueStepDecoder
  ],
  'Question',
);

export interface LogContents {
  recorded: string[];
  recordedSets: Record<string, any[]>; // eslint-disable-line
  recordedCounts: [string, number][]; // eslint-disable-line
}

export interface Campaign {
  name: string;
  id: string;
  log: LogContents;
  step: CampaignStep | null;
  difficulty: Difficulty;
}

export interface SomeRecordable {
  recordType: string;
  recordVal: any; // eslint-disable-line
}

const someRecordableDecoder = JsonDecoder.object<SomeRecordable>({
  recordType: JsonDecoder.string,
  recordVal: JsonDecoder.succeed
}, 'SomeRecordable')

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

export const campaignDecoder = JsonDecoder.object<Campaign>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  difficulty: difficultyDecoder,
  log: logContentsDecoder,
  step: JsonDecoder.nullable(campaignStepDecoder)
}, 'Campaign');

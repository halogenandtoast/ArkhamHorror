import { JsonDecoder } from 'ts.data.json';

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
}

export const scenarioStepDecoder = JsonDecoder.object<ScenarioStep>(
  {
    tag: JsonDecoder.isExactly('ScenarioStep'),
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
  recordedSets: any[]; // eslint-disable-line
  recordedCounts: [string, number][]; // eslint-disable-line
}

export interface Campaign {
  name: string;
  id: string;
  log: LogContents;
  step: CampaignStep | null;
  difficulty: string;
}

export const logContentsDecoder = JsonDecoder.object<LogContents>({
  recorded: JsonDecoder.array<string>(JsonDecoder.string, 'recorded[]'),
  recordedSets: JsonDecoder.array<any>(JsonDecoder.succeed, 'recordedSets[]'), // eslint-disable-line
  recordedCounts: JsonDecoder.array<[string, number]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.number], '[string, number]'), '[string, number][]'),
}, 'LogContents');

export const campaignDecoder = JsonDecoder.object<Campaign>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  difficulty: JsonDecoder.string,
  log: logContentsDecoder,
  step: JsonDecoder.nullable(campaignStepDecoder)
}, 'Campaign');

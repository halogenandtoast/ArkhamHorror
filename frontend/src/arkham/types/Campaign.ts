import { JsonDecoder } from 'ts.data.json';

export interface Campaign {
  tag: string;
  contents: CampaignContents;
}

export interface LogContents {
  recorded: string[];
  recordedSets: any[]; // eslint-disable-line
}

export interface CampaignContents {
  name: string;
  id: string;
  log: LogContents;
}

export const logContentsDecoder = JsonDecoder.object<LogContents>({
  recorded: JsonDecoder.array<string>(JsonDecoder.string, 'recorded[]'),
  recordedSets: JsonDecoder.array<any>(JsonDecoder.succeed, 'recordedSets[]'), // eslint-disable-line
}, 'LogContents');

export const campaignContentsDecoder = JsonDecoder.object<CampaignContents>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  log: logContentsDecoder,
}, 'CampaignContents');

export const campaignDecoder = JsonDecoder.object<Campaign>({
  tag: JsonDecoder.string,
  contents: campaignContentsDecoder,
}, 'Campaign');

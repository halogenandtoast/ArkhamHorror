import { JsonDecoder } from 'ts.data.json';

export interface Campaign {
  tag: string;
  contents: CampaignContents;
}

export interface CampaignContents {
  name: string;
  id: string;
}

export const campaignContentsDecoder = JsonDecoder.object<CampaignContents>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
}, 'CampaignContents');

export const campaignDecoder = JsonDecoder.object<Campaign>({
  tag: JsonDecoder.string,
  contents: campaignContentsDecoder,
}, 'Campaign');

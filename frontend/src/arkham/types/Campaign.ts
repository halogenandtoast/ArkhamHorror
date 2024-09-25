import { JsonDecoder } from 'ts.data.json';
import { Difficulty, difficultyDecoder } from '@/arkham/types/Difficulty';
import { LogContents, logContentsDecoder } from '@/arkham/types/Log';
import { XpBreakdown, xpBreakdownDecoder} from '@/arkham/types/Xp';
import { CampaignStep, campaignStepDecoder} from '@/arkham/types/CampaignStep';

export type CampaignDetails = {
  id: string;
  difficulty: Difficulty;
  currentCampaignMode?: string;
}

export type Campaign = {
  name: string;
  id: string;
  log: LogContents;
  step: CampaignStep | null;
  difficulty: Difficulty;
  meta: any;
  xpBreakdown: XpBreakdown;
}

export const campaignDetailsDecoder = JsonDecoder.object<CampaignDetails>({
  id: JsonDecoder.string,
  difficulty: difficultyDecoder,
  currentCampaignMode: JsonDecoder.optional(JsonDecoder.string),
}, 'CampaignDetails');

export const campaignDecoder = JsonDecoder.object<Campaign>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  difficulty: difficultyDecoder,
  log: logContentsDecoder,
  step: JsonDecoder.nullable(campaignStepDecoder),
  meta: JsonDecoder.succeed,
  xpBreakdown: xpBreakdownDecoder
}, 'Campaign');

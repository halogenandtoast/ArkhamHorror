import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { Difficulty, difficultyDecoder } from '@/arkham/types/Difficulty';
import { LogContents, logContentsDecoder } from '@/arkham/types/Log';
import { XpBreakdown, xpBreakdownDecoder} from '@/arkham/types/Xp';
import { CampaignStep, campaignStepDecoder} from '@/arkham/types/CampaignStep';
import { CardContents, cardContentsDecoder} from '@/arkham/types/Card';

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
  storyCards: { [key: string]: CardContents[] };
}

export const campaignDetailsDecoder = JsonDecoder.object<CampaignDetails>({
  id: JsonDecoder.string(),
  difficulty: difficultyDecoder,
  currentCampaignMode: v2Optional(JsonDecoder.string()),
}, 'CampaignDetails');

export const campaignDecoder = JsonDecoder.object<Campaign>({
  name: JsonDecoder.string(),
  id: JsonDecoder.string(),
  difficulty: difficultyDecoder,
  log: logContentsDecoder,
  step: JsonDecoder.nullable(campaignStepDecoder),
  meta: JsonDecoder.succeed(),
  xpBreakdown: xpBreakdownDecoder,
  storyCards: JsonDecoder.record(JsonDecoder.array(cardContentsDecoder, 'CardDef[]'), 'CardDef[]')
}, 'Campaign');

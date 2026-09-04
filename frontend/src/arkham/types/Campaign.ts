import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { Difficulty, difficultyDecoder } from '@/arkham/types/Difficulty';
import { LogContents, LogKey, logContentsDecoder, logKeyDecoder } from '@/arkham/types/Log';
import { XpBreakdown, xpBreakdownDecoder} from '@/arkham/types/Xp';
import { CampaignStep, campaignStepDecoder} from '@/arkham/types/CampaignStep';
import { CardContents, Card, cardDecoder, cardContentsDecoder} from '@/arkham/types/Card';
import { TokenFace, tokenFaceDecoder } from '@/arkham/types/ChaosToken';
import { withDefault } from '@/arkham/parser';

export type CampaignDetails = {
  id: string;
  difficulty: Difficulty;
  currentCampaignMode?: string;
}

/**
 * A recorded change to the campaign chaos bag. The bag is kept in full on both
 * sides; what was added and removed is the multiset difference between them.
 */
export type ChaosBagChange = {
  step: CampaignStep;
  before: TokenFace[];
  after: TokenFace[];
}

export const chaosBagChangeDecoder = JsonDecoder.object<ChaosBagChange>({
  step: campaignStepDecoder,
  before: JsonDecoder.array(tokenFaceDecoder, 'TokenFace[]'),
  after: JsonDecoder.array(tokenFaceDecoder, 'TokenFace[]'),
}, 'ChaosBagChange');

/**
 * A recorded change to one of the campaign log's counts (Yig's Fury, ...),
 * grouped by the campaign step it happened during.
 */
export type RecordCountChange = {
  step: CampaignStep;
  key: LogKey;
  before: number;
  after: number;
}

export const recordCountChangeDecoder = JsonDecoder.object<RecordCountChange>({
  step: campaignStepDecoder,
  key: logKeyDecoder,
  before: JsonDecoder.number(),
  after: JsonDecoder.number(),
}, 'RecordCountChange');

export type Campaign = {
  name: string;
  id: string;
  log: LogContents;
  step: CampaignStep | null;
  completedSteps: CampaignStep[] | null;
  difficulty: Difficulty;
  meta: any;
  xpBreakdown: XpBreakdown;
  storyCards: { [key: string]: Card[] };
  decks: { [key: string]: CardContents[]  };
  chaosBag: TokenFace[];
  chaosBagHistory: ChaosBagChange[];
  recordCountHistory: RecordCountChange[];
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
  completedSteps: JsonDecoder.array(campaignStepDecoder, 'CampaignStep[]'),
  meta: JsonDecoder.succeed(),
  xpBreakdown: xpBreakdownDecoder,
  storyCards: JsonDecoder.record(JsonDecoder.array(cardDecoder, 'CardDef[]'), 'CardDef[]'),
  decks: JsonDecoder.record(JsonDecoder.array(cardContentsDecoder, 'CardDef[]'), 'CardDef[]'),
  chaosBag: JsonDecoder.array(tokenFaceDecoder, 'TokenFace[]'),
  chaosBagHistory: withDefault([], JsonDecoder.array(chaosBagChangeDecoder, 'ChaosBagChange[]')),
  recordCountHistory: withDefault([], JsonDecoder.array(recordCountChangeDecoder, 'RecordCountChange[]')),
}, 'Campaign');

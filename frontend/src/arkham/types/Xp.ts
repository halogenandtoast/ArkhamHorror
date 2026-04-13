import * as JsonDecoder from 'ts.data.json';
import { CampaignStep, campaignStepDecoder} from '@/arkham/types/CampaignStep';

export type XpSource = 'XpFromVictoryDisplay' | 'XpBonus' | 'XpFromCardEffect'

export type XpDetail = {
  source : XpSource;
  sourceName: string;
  amount: number;
}

export type XpEntry
  = { tag: 'AllGainXp', details: XpDetail }
  | { tag: 'InvestigatorGainXp', investigator: string, details: XpDetail }
  | { tag: 'InvestigatorLoseXp', investigator: string, details: XpDetail }

export type XpBreakdownStep = {
  step: CampaignStep
  investigators: string[]
  entries: XpEntry[]
}

export type LegaxyXpBreakdownStep = {
  step: CampaignStep
  entries: XpEntry[]
}

export type XpBreakdown = XpBreakdownStep[];

export const xpSourceDecoder = JsonDecoder.oneOf<XpSource>(
  [
    JsonDecoder.literal('XpFromVictoryDisplay'),
    JsonDecoder.literal('XpBonus'),
    JsonDecoder.literal('XpFromCardEffect'),
  ],
  'XpSource'
);

export const xpDetailDecoder = JsonDecoder.object<XpDetail>(
  {
    source: xpSourceDecoder,
    sourceName: JsonDecoder.string(),
    amount: JsonDecoder.number()
  },
  'XpDetail',
);

export const xpEntryDecoder = JsonDecoder.oneOf<XpEntry>(
  [
    JsonDecoder.object<XpEntry>(
      {
        tag: JsonDecoder.literal('AllGainXp'),
        details: xpDetailDecoder
      },
      'AllGainXp'
    ),
    JsonDecoder.object<XpEntry>(
      {
        tag: JsonDecoder.literal('InvestigatorGainXp'),
        investigator: JsonDecoder.string(),
        details: xpDetailDecoder
      },
      'InvestigatorGainXp'
    ),
    JsonDecoder.object<XpEntry>(
      {
        tag: JsonDecoder.literal('InvestigatorLoseXp'),
        investigator: JsonDecoder.string(),
        details: xpDetailDecoder
      },
      'InvestigatorLoseXp'
    ),
  ],
  'XpEntry'
);

export const xpBreakdownStepDecoder = JsonDecoder.object<XpBreakdownStep>(
  {
    step: campaignStepDecoder,
    investigators: JsonDecoder.array(JsonDecoder.string(), 'string[]'),
    entries: JsonDecoder.array(xpEntryDecoder, 'XpEntry[]'),
  },
  'XpBreakdownStep'
);

export const legacyXpBreakdownStepDecoder = JsonDecoder.object<LegaxyXpBreakdownStep>(
  {
    step: campaignStepDecoder,
    entries: JsonDecoder.array(xpEntryDecoder, 'XpEntry[]'),
  },
  'XpBreakdownStep'
);

export const xpBreakdownDecoder =
  JsonDecoder.oneOf<XpBreakdownStep[]>([
    JsonDecoder.array(xpBreakdownStepDecoder, 'XpBreakdown'),
    JsonDecoder.array(legacyXpBreakdownStepDecoder, 'LegacyXpBreakdown').map(legacy => legacy.map(step => ({ ...step, investigators: [] } as XpBreakdownStep)))
  ], 'XpBreakdown');


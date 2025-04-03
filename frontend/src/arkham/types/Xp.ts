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

export type XpBreakdown = [CampaignStep, XpEntry[]][];

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

export const xpBreakdownDecoder =
  JsonDecoder.array (
    JsonDecoder.tuple([
      campaignStepDecoder,
      JsonDecoder.array(xpEntryDecoder, 'XpEntry[]')]
    , 'XpBreakdown')
  , 'XpBreakdown');


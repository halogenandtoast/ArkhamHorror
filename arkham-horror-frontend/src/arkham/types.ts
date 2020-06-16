import { JsonDecoder } from 'ts.data.json';

export type ArkhamCycle = 'NightOfTheZealot' | 'TheDunwichLegacy';

export const arkhamCycleDecoder = JsonDecoder.oneOf<ArkhamCycle>([
  JsonDecoder.isExactly('NightOfTheZealot'),
  JsonDecoder.isExactly('TheDunwichLegacy'),
], 'ArkhamCycle');

export type ArkhamScenario = 'ScenarioOne' | 'ScenarioTwo';

export const arkhamScenarioDecoder = JsonDecoder.oneOf<ArkhamScenario>([
  JsonDecoder.isExactly('ScenarioOne'),
  JsonDecoder.isExactly('ScenarioTwo'),
], 'ArkhamCycle');

export interface ArkhamCard {
  cost: number;
  image: string;
}

export const arkhamCardDecoder = JsonDecoder.object<ArkhamCard>(
  {
    cost: JsonDecoder.number,
    image: JsonDecoder.string,
  },
  'ArkhamCard',
);

export interface ArkhamInvestigator {
  investigatorName: string;
  investigatorImage: string;
}

export const arkhamInvestigatorDecoder = JsonDecoder.object<ArkhamInvestigator>(
  {
    investigatorName: JsonDecoder.string,
    investigatorImage: JsonDecoder.string,
  },
  'ArkhamInvestigator',
);

export interface ArkhamPlayer {
  investigator: ArkhamInvestigator;
  sanityDamage: number;
  healthDamage: number;
  resources: number;
  clues: number;
  hand: ArkhamCard[];
  inPlay: ArkhamCard[];
}

export const arkhamPlayerDecoder = JsonDecoder.object<ArkhamPlayer>(
  {
    investigator: arkhamInvestigatorDecoder,
    sanityDamage: JsonDecoder.number,
    healthDamage: JsonDecoder.number,
    resources: JsonDecoder.number,
    clues: JsonDecoder.number,
    hand: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
    inPlay: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
  },
  'ArkhamPlayer',
);

export type ArkhamChaosToken = '+1' | '0' | '-1' | '-2' | '-3' | '-4' | '-5' | '-6' | '-7' | '-8' | 'skull' | 'cultist' | 'tablet' | 'elderthing' | 'autofail' | 'eldersign'

export const arkhamChaosTokenDecoder = JsonDecoder.oneOf<ArkhamChaosToken>([
  JsonDecoder.isExactly('+1'),
  JsonDecoder.isExactly('0'),
  JsonDecoder.isExactly('-1'),
  JsonDecoder.isExactly('-2'),
  JsonDecoder.isExactly('-3'),
  JsonDecoder.isExactly('-4'),
  JsonDecoder.isExactly('-5'),
  JsonDecoder.isExactly('-6'),
  JsonDecoder.isExactly('-7'),
  JsonDecoder.isExactly('-8'),
  JsonDecoder.isExactly('skull'),
  JsonDecoder.isExactly('cultist'),
  JsonDecoder.isExactly('tablet'),
  JsonDecoder.isExactly('elderthing'),
  JsonDecoder.isExactly('autofail'),
  JsonDecoder.isExactly('eldersign'),
], 'ArkhamChaosToken');

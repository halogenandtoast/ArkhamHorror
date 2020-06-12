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
  hand: ArkhamCard[];
  inPlay: ArkhamCard[];
}

export const arkhamPlayerDecoder = JsonDecoder.object<ArkhamPlayer>(
  {
    investigator: arkhamInvestigatorDecoder,
    sanityDamage: JsonDecoder.number,
    healthDamage: JsonDecoder.number,
    resources: JsonDecoder.number,
    hand: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
    inPlay: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
  },
  'ArkhamPlayer',
);

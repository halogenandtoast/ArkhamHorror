import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamCard,
  arkhamCardDecoder,
} from '@/arkham/types/card';

export type ArkhamCycle = 'NightOfTheZealot' | 'TheDunwichLegacy';

export const arkhamCycleDecoder = JsonDecoder.oneOf<ArkhamCycle>([
  JsonDecoder.isExactly('NightOfTheZealot'),
  JsonDecoder.isExactly('TheDunwichLegacy'),
], 'ArkhamCycle');

export interface ArkhamScenario {
  name: string;
  guide: string;
}

export const arkhamScenarioDecoder = JsonDecoder.object<ArkhamScenario>({
  name: JsonDecoder.string,
  guide: JsonDecoder.string,
}, 'ArkhamCycle');

export interface ArkhamInvestigator {
  name: string;
  image: string;
  portrait: string;
  willpower: number;
  intellect: number;
  combat: number;
  agility: number;
}

export const arkhamInvestigatorDecoder = JsonDecoder.object<ArkhamInvestigator>(
  {
    name: JsonDecoder.string,
    image: JsonDecoder.string,
    portrait: JsonDecoder.string,
    willpower: JsonDecoder.number,
    intellect: JsonDecoder.number,
    combat: JsonDecoder.number,
    agility: JsonDecoder.number,
  },
  'ArkhamInvestigator',
);

export interface ArkhamPlayer {
  investigator: ArkhamInvestigator;
  sanityDamage: number;
  healthDamage: number;
  resources: number;
  clues: number;
  actionsRemaining: number;
  hand: ArkhamCard[];
  inPlay: ArkhamCard[];
  enemies: string[];
  discard: ArkhamCard[];
}

export const arkhamPlayerDecoder = JsonDecoder.object<ArkhamPlayer>(
  {
    investigator: arkhamInvestigatorDecoder,
    sanityDamage: JsonDecoder.number,
    healthDamage: JsonDecoder.number,
    resources: JsonDecoder.number,
    clues: JsonDecoder.number,
    actionsRemaining: JsonDecoder.number,
    hand: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
    inPlay: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
    enemies: JsonDecoder.array<string>(JsonDecoder.string, 'UUID[]'),
    discard: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
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

// ArkhamEnemu
export interface ArkhamEnemy {
  id: string;
  combat: number;
  health: number;
  agility: number;
  healthDamage: number;
  sanityDamage: number;
  victory: number | null;
  cardCode: string;
  isHunter: boolean;
  isEngaged: boolean;
  image: string;
}

export const arkhamEnemyDecoder = JsonDecoder.object<ArkhamEnemy>(
  {
    id: JsonDecoder.string,
    combat: JsonDecoder.number,
    health: JsonDecoder.number,
    agility: JsonDecoder.number,
    healthDamage: JsonDecoder.number,
    sanityDamage: JsonDecoder.number,
    victory: JsonDecoder.nullable(JsonDecoder.number),
    cardCode: JsonDecoder.string,
    isHunter: JsonDecoder.boolean,
    isEngaged: JsonDecoder.boolean,
    image: JsonDecoder.string,
  },
  'ArkhamEnemy',
);

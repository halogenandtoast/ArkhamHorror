import { JsonDecoder } from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';
import { BreachStatus, breachStatusDecoder } from '@/arkham/types/Breach';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';

export type Brazier = 'Lit' | 'Unlit';

export const brazierDecoder: JsonDecoder.Decoder<Brazier> = JsonDecoder.oneOf<Brazier>([
  JsonDecoder.isExactly('Lit'),
  JsonDecoder.isExactly('Unlit'),
], 'Brazier');


export type FloodLevel = "Unflooded" | "PartiallyFlooded" | "FullyFlooded"

export const floodLevelDecoder: JsonDecoder.Decoder<FloodLevel> = JsonDecoder.oneOf<FloodLevel>([
  JsonDecoder.isExactly('Unflooded'),
  JsonDecoder.isExactly('PartiallyFlooded'),
  JsonDecoder.isExactly('FullyFlooded'),
], 'FloodLevel');

export type Location = {
  cardCode: string;
  label: string;
  id: number;
  cardId: number;
  tokens: Tokens;
  shroud: number;
  revealed: boolean;
  investigators: string[];
  enemies: number[];
  treacheries: number[];
  assets: number[];
  events: number[];
  cardsUnderneath: Card[];
  modifiers: Modifier[];
  connectedLocations: number[];
  inFrontOf: string | null;
  brazier: Brazier | null;
  breaches: BreachStatus | null;
  floodLevel: FloodLevel | null;
  keys: ArkhamKey[];
}

export const locationDecoder = JsonDecoder.object<Location>(
  {
    cardCode: JsonDecoder.string,
    label: JsonDecoder.string,
    id: JsonDecoder.number,
    cardId: JsonDecoder.number,
    tokens: tokensDecoder,
    shroud: JsonDecoder.number,
    revealed: JsonDecoder.boolean,
    investigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorId[]'),
    enemies: JsonDecoder.array<number>(JsonDecoder.number, 'EnemyId[]'),
    treacheries: JsonDecoder.array<number>(JsonDecoder.number, 'TreacheryId[]'),
    assets: JsonDecoder.array<number>(JsonDecoder.number, 'AssetId[]'),
    events: JsonDecoder.array<number>(JsonDecoder.number, 'EventId[]'),
    cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'UnderneathCard[]'),
    modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
    connectedLocations: JsonDecoder.array<number>(JsonDecoder.number, 'LocationId[]'),
    inFrontOf: JsonDecoder.nullable(JsonDecoder.string),
    brazier: JsonDecoder.nullable(brazierDecoder),
    breaches: JsonDecoder.nullable(breachStatusDecoder),
    floodLevel: JsonDecoder.nullable(floodLevelDecoder),
    keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  },
  'Location',
);

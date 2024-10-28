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
  id: string;
  cardId: string;
  tokens: Tokens;
  shroud: number;
  revealed: boolean;
  investigators: string[];
  enemies: string[];
  treacheries: string[];
  assets: string[];
  events: string[];
  cardsUnderneath: Card[];
  modifiers: Modifier[];
  connectedLocations: string[];
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
    id: JsonDecoder.string,
    cardId: JsonDecoder.string,
    tokens: tokensDecoder,
    shroud: JsonDecoder.number,
    revealed: JsonDecoder.boolean,
    investigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorId[]'),
    enemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
    treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
    assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
    events: JsonDecoder.array<string>(JsonDecoder.string, 'EventId[]'),
    cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'UnderneathCard[]'),
    modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
    connectedLocations: JsonDecoder.array<string>(JsonDecoder.string, 'LocationId[]'),
    inFrontOf: JsonDecoder.nullable(JsonDecoder.string),
    brazier: JsonDecoder.nullable(brazierDecoder),
    breaches: JsonDecoder.nullable(breachStatusDecoder),
    floodLevel: JsonDecoder.nullable(floodLevelDecoder),
    keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  },
  'Location',
);

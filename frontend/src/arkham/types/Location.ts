import { JsonDecoder } from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';

export type Brazier = 'Lit' | 'Unlit';

export const brazierDecoder: JsonDecoder.Decoder<Brazier> = JsonDecoder.oneOf<Brazier>([
  JsonDecoder.isExactly('Lit'),
  JsonDecoder.isExactly('Unlit'),
], 'Brazier');

export type BreachStatus = { tag: "Breaches", contents: number } | { tag: "Incursion", contents: number }

export const breachStatusDecoder: JsonDecoder.Decoder<BreachStatus> = JsonDecoder.oneOf<BreachStatus>([
  JsonDecoder.object<BreachStatus>({ tag: JsonDecoder.isExactly("Breaches"), contents: JsonDecoder.number }, 'Breaches'),
  JsonDecoder.object<BreachStatus>({ tag: JsonDecoder.isExactly("Incursion"), contents: JsonDecoder.number }, 'Incursion'),
], 'BreachStatus');

export type Location = {
  cardCode: string;
  label: string;
  id: string;
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
  keys: ArkhamKey[];
}

export const locationDecoder = JsonDecoder.object<Location>(
  {
    cardCode: JsonDecoder.string,
    label: JsonDecoder.string,
    id: JsonDecoder.string,
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
    keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  },
  'Location',
);

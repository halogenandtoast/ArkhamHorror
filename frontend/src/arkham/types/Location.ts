import { JsonDecoder } from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';

export interface Location {
  cardCode: string;
  label: string;
  id: string;
  clues: number;
  doom: number;
  horror: number;
  resources: number;
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
  keys: ArkhamKey[];
}

export const locationDecoder = JsonDecoder.object<Location>(
  {
    cardCode: JsonDecoder.string,
    label: JsonDecoder.string,
    id: JsonDecoder.string,
    clues: JsonDecoder.number,
    doom: JsonDecoder.number,
    horror: JsonDecoder.number,
    resources: JsonDecoder.number,
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
    keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  },
  'Location',
);

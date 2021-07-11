import { JsonDecoder } from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';

export interface Location {
  tag: string;
  contents: LocationContents;
  modifiers: Modifier[];
}

export interface LocationContents {
  cardDef: CardDef;
  label: string;
  id: string;
  clues: number;
  doom: number;
  shroud: number;
  revealed: boolean;
  investigators: string[];
  enemies: string[];
  connectedLocations: string[];
  treacheries: string[];
  assets: string[];
  cardsUnderneath: Card[];
}

export const locationContentsDecoder = JsonDecoder.object<LocationContents>(
  {
    cardDef: cardDefDecoder,
    label: JsonDecoder.string,
    id: JsonDecoder.string,
    clues: JsonDecoder.number,
    doom: JsonDecoder.number,
    shroud: JsonDecoder.number,
    revealed: JsonDecoder.boolean,
    investigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorId[]'),
    enemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
    connectedLocations: JsonDecoder.array<string>(JsonDecoder.string, 'LocationId[]'),
    treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
    assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
    cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'UnderneathCard[]'),
  },
  'Attrs',
);

export const locationDecoder = JsonDecoder.object<Location>({
  tag: JsonDecoder.string,
  contents: locationContentsDecoder,
  modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
}, 'Location');

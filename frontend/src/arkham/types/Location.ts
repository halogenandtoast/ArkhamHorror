import { JsonDecoder } from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';

interface ModifierType {
  tag: string;
}

interface Modifier {
  type: ModifierType;
}

const modifierTypeDecoder = JsonDecoder.object<ModifierType>({
  tag: JsonDecoder.string
}, 'ModifierType')

const modifierDecoder = JsonDecoder.object<Modifier>({
  type: modifierTypeDecoder
}, 'Modifier')

export interface Location {
  tag: string;
  contents: LocationContents;
  modifiers: Modifier[];
}

export interface LocationName {
  title: string;
  subtitle: string | null;
}

export const locationNameDecoder = JsonDecoder.object<LocationName>(
  {
    title: JsonDecoder.string,
    subtitle: JsonDecoder.nullable(JsonDecoder.string),
  },
  'LocationName'
);

export interface LocationContents {
  name: LocationName;
  label: string;
  id: string;
  clues: number;
  shroud: number;
  revealed: boolean;
  investigators: string[];
  enemies: string[];
  victory: number | null;
  connectedLocations: string[];
  treacheries: string[];
  assets: string[];
  cardsUnderneath: Card[];
}

export const locationContentsDecoder = JsonDecoder.object<LocationContents>(
  {
    name: locationNameDecoder,
    label: JsonDecoder.string,
    id: JsonDecoder.string,
    clues: JsonDecoder.number,
    shroud: JsonDecoder.number,
    revealed: JsonDecoder.boolean,
    investigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorId[]'),
    enemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
    victory: JsonDecoder.nullable(JsonDecoder.number),
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

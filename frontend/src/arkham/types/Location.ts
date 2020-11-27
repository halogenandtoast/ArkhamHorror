import { JsonDecoder } from 'ts.data.json';

export interface Location {
  tag: string;
  contents: LocationContents;
}

export interface LocationContents {
  name: string;
  label: string;
  id: string;
  clues: number;
  shroud: number;
  revealed: boolean;
  investigators: string[];
  enemies: string[];
  victory: number | null;
  // symbol: LocationSymbol;
  // connectedSymbols: HashSet LocationSymbol;
  connectedLocations: string[];
  treacheries: string[];
  assets: string[];
}

export const locationContentsDecoder = JsonDecoder.object<LocationContents>(
  {
    name: JsonDecoder.string,
    label: JsonDecoder.string,
    id: JsonDecoder.string,
    clues: JsonDecoder.number,
    shroud: JsonDecoder.number,
    revealed: JsonDecoder.boolean,
    investigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorId[]'),
    enemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
    victory: JsonDecoder.nullable(JsonDecoder.number),
    // symbol: LocationSymbol,
    // connectedSymbols: HashSet LocationSymbol,
    connectedLocations: JsonDecoder.array<string>(JsonDecoder.string, 'LocationId[]'),
    treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
    assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  },
  'Attrs',
);

export const locationDecoder = JsonDecoder.object<Location>({
  tag: JsonDecoder.string,
  contents: locationContentsDecoder,
}, 'Location');

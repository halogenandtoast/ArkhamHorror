import { JsonDecoder } from 'ts.data.json';

export type ArkhamLocationSymbol = 'Circle' | 'Heart';

export const arkhamLocationSymbolDecoder = JsonDecoder.oneOf<ArkhamLocationSymbol>([
  JsonDecoder.isExactly('Circle'),
  JsonDecoder.isExactly('Heart'),
], 'ArkhamLocationSymbol');

export type ArkhamLocation = ArkhamRevealedLocation | ArkhamUnrevealedLocation

export interface ArkhamUnrevealedLocationContent {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  image: string;
  locationId: string;
}

export interface ArkhamRevealedLocation {
  tag: 'RevealedLocation';
  contents: ArkhamRevealedLocationContent;
}

export interface ArkhamUnrevealedLocation {
  tag: 'UnrevealedLocation';
  contents: ArkhamUnrevealedLocationContent;
}

export const arkhamUnrevealedLocationDecoder = JsonDecoder.object<ArkhamUnrevealedLocationContent>(
  {
    name: JsonDecoder.string,
    locationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    image: JsonDecoder.string,
    locationId: JsonDecoder.string,
  },
  'ArkhamUnrevealedLocation',
);

export interface ArkhamRevealedLocationContent {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  shroud: number;
  image: string;
  locationId: string;
}

export const arkhamRevealedLocationDecoder = JsonDecoder.object<ArkhamRevealedLocationContent>(
  {
    name: JsonDecoder.string,
    locationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    shroud: JsonDecoder.number,
    image: JsonDecoder.string,
    locationId: JsonDecoder.string,
  },
  'ArkhamUnrevealedLocation',
);

export const arkhamLocationRevealedLocationDecoder = JsonDecoder.object<
    ArkhamLocation
  >(
    {
      tag: JsonDecoder.isExactly('RevealedLocation'),
      contents: arkhamRevealedLocationDecoder,
    },
    'ArkhamLocation<ArkhamRevealedLocation>',
  );

export const arkhamLocationUnrevealedLocationDecoder = JsonDecoder.object<
    ArkhamLocation
  >(
    {
      tag: JsonDecoder.isExactly('UnrevealedLocation'),
      contents: arkhamUnrevealedLocationDecoder,
    },
    'ArkhamLocation<ArkhamUnrevealedLocation>',
  );

export const arkhamLocationDecoder = JsonDecoder.oneOf<ArkhamLocation>([
  arkhamLocationUnrevealedLocationDecoder,
  arkhamLocationRevealedLocationDecoder,
], 'ArkhamLocation');

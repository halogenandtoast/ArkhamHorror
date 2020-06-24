import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamInvestigator,
  arkhamInvestigatorDecoder,
} from '@/arkham/types';

export type ArkhamLocationSymbol = 'Circle' | 'Heart';

export const arkhamLocationSymbolDecoder = JsonDecoder.oneOf<ArkhamLocationSymbol>([
  JsonDecoder.isExactly('Circle'),
  JsonDecoder.isExactly('Heart'),
], 'ArkhamLocationSymbol');

export type ArkhamLocation = ArkhamRevealedLocation | ArkhamUnrevealedLocation

type LocationClues = {
  tag: 'LocationClues';
  contents: number;
}

type LocationInvestigator = {
  tag: 'LocationInvestigator';
  contents: ArkhamInvestigator;
}

type LocationContent = LocationClues | LocationInvestigator;

export const arkhamLocationContentLocationCluesDecoder = JsonDecoder.object<LocationContent>({
  tag: JsonDecoder.isExactly('LocationClues'),
  contents: JsonDecoder.number,
}, 'LocationClues');

export const arkhamLocationContentLocationInvestigatorDecoder = JsonDecoder.object<
    LocationContent
  >({
    tag: JsonDecoder.isExactly('LocationInvestigator'),
    contents: arkhamInvestigatorDecoder,
  }, 'LocationInvestigator');

export const arkhamLocationContentDecoder = JsonDecoder.oneOf<LocationContent>([
  arkhamLocationContentLocationCluesDecoder,
  arkhamLocationContentLocationInvestigatorDecoder,
], 'LocationContent');


export interface ArkhamUnrevealedLocationContent {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  image: string;
  locationId: string;
  contents: LocationContent[];
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
    contents: JsonDecoder.array<LocationContent>(arkhamLocationContentDecoder, 'LocationContent[]'),
  },
  'ArkhamUnrevealedLocation',
);

export interface ArkhamRevealedLocationContent {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  shroud: number;
  image: string;
  locationId: string;
  contents: LocationContent[];
}

export const arkhamRevealedLocationDecoder = JsonDecoder.object<ArkhamRevealedLocationContent>(
  {
    name: JsonDecoder.string,
    locationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    shroud: JsonDecoder.number,
    image: JsonDecoder.string,
    locationId: JsonDecoder.string,
    contents: JsonDecoder.array<LocationContent>(arkhamLocationContentDecoder, 'LocationContent[]'),
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

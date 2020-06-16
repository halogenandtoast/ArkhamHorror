import { JsonDecoder } from 'ts.data.json';

export type ArkhamLocationSymbol = 'Circle' | 'Heart';

export const arkhamLocationSymbolDecoder = JsonDecoder.oneOf<ArkhamLocationSymbol>([
  JsonDecoder.isExactly('Circle'),
  JsonDecoder.isExactly('Heart'),
], 'ArkhamLocationSymbol');

export type ArkhamLocation<T> = {
  tag: string;
  contents: T;
}

export interface ArkhamUnrevealedLocation {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  image: string;
}

export const arkhamUnrevealedLocationDecoder = JsonDecoder.object<ArkhamUnrevealedLocation>(
  {
    name: JsonDecoder.string,
    locationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    image: JsonDecoder.string,
  },
  'ArkhamUnrevealedLocation',
);

export interface ArkhamRevealedLocation {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  shroud: number;
  image: string;
}

export const arkhamRevealedLocationDecoder = JsonDecoder.object<ArkhamRevealedLocation>(
  {
    name: JsonDecoder.string,
    locationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    shroud: JsonDecoder.number,
    image: JsonDecoder.string,
  },
  'ArkhamUnrevealedLocation',
);

export const arkhamLocationRevealedLocationDecoder = JsonDecoder.object<
    ArkhamLocation<ArkhamRevealedLocation>
  >(
    {
      tag: JsonDecoder.isExactly('RevealedLocation'),
      contents: arkhamRevealedLocationDecoder,
    },
    'ArkhamLocation<ArkhamRevealedLocation>',
  );

export const arkhamLocationUnrevealedLocationDecoder = JsonDecoder.object<
    ArkhamLocation<ArkhamUnrevealedLocation>
  >(
    {
      tag: JsonDecoder.isExactly('UnrevealedLocation'),
      contents: arkhamUnrevealedLocationDecoder,
    },
    'ArkhamLocation<ArkhamUnrevealedLocation>',
  );

export const arkhamLocationDecoder = JsonDecoder.object<
    ArkhamLocation<ArkhamUnrevealedLocation | ArkhamRevealedLocation>
  >(
    {
      tag: JsonDecoder.string,
      contents: JsonDecoder.succeed,
    },
    'ArkhamLocation',
  ).then((value) => {
    switch (value.tag) {
      case 'RevealedLocation':
        return arkhamLocationRevealedLocationDecoder;
      case 'UnrevealedLocation':
        return arkhamLocationUnrevealedLocationDecoder;
      default:
        return JsonDecoder.fail<ArkhamLocation<ArkhamUnrevealedLocation | ArkhamRevealedLocation>>(
          `<ArkhamLocation> does not support tag ${value.tag}`,
        );
    }
  }).map((value) => value.contents);

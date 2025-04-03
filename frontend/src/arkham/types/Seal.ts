import * as JsonDecoder from 'ts.data.json';

export type SealKind = 'SealA' | 'SealB' | 'SealC' | 'SealD' | 'SealE';
export const sealKindDecoder: JsonDecoder.Decoder<SealKind> = JsonDecoder.oneOf<SealKind>([
  JsonDecoder.literal('SealA'),
  JsonDecoder.literal('SealB'),
  JsonDecoder.literal('SealC'),
  JsonDecoder.literal('SealD'),
  JsonDecoder.literal('SealE'),
], 'SealKind');

export type Seal =
  { sealKind: SealKind
  , sealActive: boolean
  }

export const sealDecoder: JsonDecoder.Decoder<Seal> = JsonDecoder.object<Seal>(
  { sealKind: sealKindDecoder
  , sealActive: JsonDecoder.boolean()
  }, 'Seal')

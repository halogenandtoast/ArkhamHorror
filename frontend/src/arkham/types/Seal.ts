import { JsonDecoder } from 'ts.data.json';

export type SealKind = 'SealA' | 'SealB' | 'SealC' | 'SealD' | 'SealE';
export const sealKindDecoder: JsonDecoder.Decoder<SealKind> = JsonDecoder.oneOf<SealKind>([
  JsonDecoder.isExactly('SealA'),
  JsonDecoder.isExactly('SealB'),
  JsonDecoder.isExactly('SealC'),
  JsonDecoder.isExactly('SealD'),
  JsonDecoder.isExactly('SealE'),
], 'SealKind');

export type Seal =
  { sealKind: SealKind
  , sealActive: boolean
  }

export const sealDecoder: JsonDecoder.Decoder<Seal> = JsonDecoder.object<Seal>(
  { sealKind: sealKindDecoder
  , sealActive: JsonDecoder.boolean
  }, 'Seal')

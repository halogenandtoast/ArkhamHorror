import { JsonDecoder } from 'ts.data.json';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

export interface ChaosToken {
  face: TokenFace;
  id: string;
  modifiers?: Modifier[];
}

export type TokenFace = 'PlusOne' | 'Zero' | 'MinusOne' | 'MinusTwo' | 'MinusThree' | 'MinusFour' | 'MinusFive' | 'MinusSix' | 'MinusSeven' | 'MinusEight' | 'Skull' | 'Cultist' | 'Tablet' | 'ElderThing' | 'AutoFail' | 'ElderSign' | 'CurseToken' | 'BlessToken'

export const tokenFaceDecoder = JsonDecoder.oneOf<TokenFace>([
  JsonDecoder.isExactly('PlusOne'),
  JsonDecoder.isExactly('Zero'),
  JsonDecoder.isExactly('MinusOne'),
  JsonDecoder.isExactly('MinusTwo'),
  JsonDecoder.isExactly('MinusThree'),
  JsonDecoder.isExactly('MinusFour'),
  JsonDecoder.isExactly('MinusFive'),
  JsonDecoder.isExactly('MinusSix'),
  JsonDecoder.isExactly('MinusSeven'),
  JsonDecoder.isExactly('MinusEight'),
  JsonDecoder.isExactly('Skull'),
  JsonDecoder.isExactly('Cultist'),
  JsonDecoder.isExactly('Tablet'),
  JsonDecoder.isExactly('ElderThing'),
  JsonDecoder.isExactly('AutoFail'),
  JsonDecoder.isExactly('ElderSign'),
  JsonDecoder.isExactly('CurseToken'),
  JsonDecoder.isExactly('BlessToken'),
], 'TokenFace');

export const chaosTokenDecoder = JsonDecoder.object<ChaosToken>({
  id: JsonDecoder.string,
  face: tokenFaceDecoder,
  modifiers: JsonDecoder.optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
}, 'ChaosToken', { id: 'chaosTokenId', face: 'chaosTokenFace' });

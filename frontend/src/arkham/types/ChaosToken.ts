import * as JsonDecoder from 'ts.data.json';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { imgsrc } from '@/arkham/helpers';
import { v2Optional } from '@/arkham/parser';

export type ChaosToken = {
  face: TokenFace;
  id: string;
  modifiers?: Modifier[];
}

export type TokenFace = 'PlusOne' | 'Zero' | 'MinusOne' | 'MinusTwo' | 'MinusThree' | 'MinusFour' | 'MinusFive' | 'MinusSix' | 'MinusSeven' | 'MinusEight' | 'Skull' | 'Cultist' | 'Tablet' | 'ElderThing' | 'AutoFail' | 'ElderSign' | 'CurseToken' | 'BlessToken' | 'FrostToken'

export const tokenFaceDecoder = JsonDecoder.oneOf<TokenFace>([
  JsonDecoder.literal('PlusOne'),
  JsonDecoder.literal('Zero'),
  JsonDecoder.literal('MinusOne'),
  JsonDecoder.literal('MinusTwo'),
  JsonDecoder.literal('MinusThree'),
  JsonDecoder.literal('MinusFour'),
  JsonDecoder.literal('MinusFive'),
  JsonDecoder.literal('MinusSix'),
  JsonDecoder.literal('MinusSeven'),
  JsonDecoder.literal('MinusEight'),
  JsonDecoder.literal('Skull'),
  JsonDecoder.literal('Cultist'),
  JsonDecoder.literal('Tablet'),
  JsonDecoder.literal('ElderThing'),
  JsonDecoder.literal('AutoFail'),
  JsonDecoder.literal('ElderSign'),
  JsonDecoder.literal('CurseToken'),
  JsonDecoder.literal('BlessToken'),
  JsonDecoder.literal('FrostToken'),
], 'TokenFace');

export const chaosTokenDecoder = JsonDecoder.object({
  chaosTokenId: JsonDecoder.string(),
  chaosTokenFace: tokenFaceDecoder,
  modifiers: v2Optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
}, 'ChaosToken').map(({chaosTokenId, chaosTokenFace, modifiers}) => ({ id: chaosTokenId, face: chaosTokenFace, modifiers }));

export function chaosTokenImage(face: TokenFace): string {
  switch (face) {
    case 'PlusOne':
      return imgsrc("ct_plus1.png");
    case 'Zero':
      return imgsrc("ct_0.png");
    case 'MinusOne':
      return imgsrc("ct_minus1.png");
    case 'MinusTwo':
      return imgsrc("ct_minus2.png");
    case 'MinusThree':
      return imgsrc("ct_minus3.png");
    case 'MinusFour':
      return imgsrc("ct_minus4.png");
    case 'MinusFive':
      return imgsrc("ct_minus5.png");
    case 'MinusSix':
      return imgsrc("ct_minus6.png");
    case 'MinusSeven':
      return imgsrc("ct_minus7.png");
    case 'MinusEight':
      return imgsrc("ct_minus8.png");
    case 'AutoFail':
      return imgsrc("ct_autofail.png");
    case 'ElderSign':
      return imgsrc("ct_eldersign.png");
    case 'Skull':
      return imgsrc("ct_skull.png");
    case 'Cultist':
      return imgsrc("ct_cultist.png");
    case 'Tablet':
      return imgsrc("ct_tablet.png");
    case 'ElderThing':
      return imgsrc("ct_elderthing.png");
    case 'CurseToken':
      return imgsrc("ct_curse.png");
    case 'BlessToken':
      return imgsrc("ct_bless.png");
    case 'FrostToken':
      return imgsrc("ct_frost.png");
    default:
      return imgsrc("ct_blank.png");
  }
}


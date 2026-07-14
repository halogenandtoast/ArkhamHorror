import * as JsonDecoder from 'ts.data.json';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { imgsrc } from '@/arkham/helpers';
import { v2Optional } from '@/arkham/parser';

export type ChaosToken = {
  face: TokenFace;
  id: string;
  modifiers?: Modifier[];
}

export const tokenOrder = [
  'PlusOne', 'Zero',
  'MinusOne', 'MinusTwo', 'MinusThree', 'MinusFour', 'MinusFive', 'MinusSix', 'MinusSeven', 'MinusEight',
  'Skull', 'Cultist', 'Tablet', 'ElderThing',
  'AutoFail', 'ElderSign',
  'CurseToken', 'BlessToken', 'FrostToken',
] as const

// Custom homebrew tokens arrive as slugs like ":circus-ex-mortis:moon"; the
// segment after the last colon is the icon key (ct_<key>.png).
export type TokenFace = typeof tokenOrder[number] | string

export function customTokenKey(face: string): string | null {
  if (!face.includes(':')) return null
  const parts = face.split(':')
  return parts[parts.length - 1] ?? null
}

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
  JsonDecoder.string(), // custom homebrew token slug
], 'TokenFace');

export const chaosTokenDecoder = JsonDecoder.object({
  chaosTokenId: JsonDecoder.string(),
  chaosTokenFace: tokenFaceDecoder,
  modifiers: v2Optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
}, 'ChaosToken').map(({chaosTokenId, chaosTokenFace, modifiers}) => ({ id: chaosTokenId, face: chaosTokenFace, modifiers }));

export function chaosTokenImage(face: TokenFace): string {
  switch (face) {
    case 'PlusOne':
      return imgsrc("chaos-tokens/ct_plus1.png");
    case 'Zero':
      return imgsrc("chaos-tokens/ct_0.png");
    case 'MinusOne':
      return imgsrc("chaos-tokens/ct_minus1.png");
    case 'MinusTwo':
      return imgsrc("chaos-tokens/ct_minus2.png");
    case 'MinusThree':
      return imgsrc("chaos-tokens/ct_minus3.png");
    case 'MinusFour':
      return imgsrc("chaos-tokens/ct_minus4.png");
    case 'MinusFive':
      return imgsrc("chaos-tokens/ct_minus5.png");
    case 'MinusSix':
      return imgsrc("chaos-tokens/ct_minus6.png");
    case 'MinusSeven':
      return imgsrc("chaos-tokens/ct_minus7.png");
    case 'MinusEight':
      return imgsrc("chaos-tokens/ct_minus8.png");
    case 'AutoFail':
      return imgsrc("chaos-tokens/ct_autofail.png");
    case 'ElderSign':
      return imgsrc("chaos-tokens/ct_eldersign.png");
    case 'Skull':
      return imgsrc("chaos-tokens/ct_skull.png");
    case 'Cultist':
      return imgsrc("chaos-tokens/ct_cultist.png");
    case 'Tablet':
      return imgsrc("chaos-tokens/ct_tablet.png");
    case 'ElderThing':
      return imgsrc("chaos-tokens/ct_elderthing.png");
    case 'CurseToken':
      return imgsrc("chaos-tokens/ct_curse.png");
    case 'BlessToken':
      return imgsrc("chaos-tokens/ct_bless.png");
    case 'FrostToken':
      return imgsrc("chaos-tokens/ct_frost.png");
    default: {
      if (face.includes(':')) {
        const [, campaign, key] = face.split(':')
        if (campaign && key) return imgsrc(`homebrew/${campaign}/chaos-tokens/${key}.png`)
      }
      return imgsrc("chaos-tokens/ct_blank.png");
    }
  }
}


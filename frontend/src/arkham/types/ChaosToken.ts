import * as JsonDecoder from 'ts.data.json';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { imgsrc } from '@/arkham/helpers';
import { v2Optional } from '@/arkham/parser';

export type ChaosToken = {
  face: TokenFace;
  id: string;
  modifiers?: Modifier[];
  modifiedFaces?: TokenFace[];
}

export const tokenOrder = [
  'PlusOne', 'Zero',
  'MinusOne', 'MinusTwo', 'MinusThree', 'MinusFour', 'MinusFive', 'MinusSix', 'MinusSeven', 'MinusEight',
  'Skull', 'Cultist', 'Tablet', 'ElderThing',
  'AutoFail', 'ElderSign',
  'CurseToken', 'BlessToken', 'FrostToken', 'BloodToken'
] as const

// Custom homebrew tokens arrive as slugs like ":circus-ex-mortis:moon"; the
// segment after the last colon is the icon key (ct_<key>.png).
export type TokenFace = typeof tokenOrder[number] | string

const tokenOrderIndex = new Map<string, number>(tokenOrder.map((face, index) => [face, index]))

/**
 * The single source of truth for chaos token ordering. Faces we don't know
 * about (custom homebrew slugs) sort after every known face.
 */
export function compareTokenFaces(a: TokenFace, b: TokenFace): number {
  return (tokenOrderIndex.get(a) ?? tokenOrder.length) - (tokenOrderIndex.get(b) ?? tokenOrder.length)
}

/** The standard chaos tokens, i.e. everything before the campaign-specific ones. */
export const standardTokenFaces: readonly TokenFace[] = tokenOrder.slice(0, tokenOrderIndex.get('CurseToken'))

/** Faces whose art already states their value. */
export const numericTokenFaces: readonly TokenFace[] = tokenOrder.slice(0, tokenOrderIndex.get('Skull'))

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
  JsonDecoder.literal('BloodToken'),
  JsonDecoder.string(), // custom homebrew token slug
], 'TokenFace');

export const chaosTokenDecoder = JsonDecoder.object({
  chaosTokenId: JsonDecoder.string(),
  chaosTokenFace: tokenFaceDecoder,
  modifiers: v2Optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
  modifiedFaces: v2Optional(JsonDecoder.array<TokenFace>(tokenFaceDecoder, 'TokenFace[]')),
}, 'ChaosToken').map(({chaosTokenId, chaosTokenFace, modifiers, modifiedFaces}) => ({
  id: chaosTokenId,
  face: chaosTokenFace,
  modifiers,
  modifiedFaces,
}));

// Every face in `tokenOrder` must name an image here, so adding a token face
// fails to compile until its art is wired up.
const tokenImageNames: Record<typeof tokenOrder[number], string> = {
  PlusOne: 'plus1',
  Zero: '0',
  MinusOne: 'minus1',
  MinusTwo: 'minus2',
  MinusThree: 'minus3',
  MinusFour: 'minus4',
  MinusFive: 'minus5',
  MinusSix: 'minus6',
  MinusSeven: 'minus7',
  MinusEight: 'minus8',
  Skull: 'skull',
  Cultist: 'cultist',
  Tablet: 'tablet',
  ElderThing: 'elderthing',
  AutoFail: 'autofail',
  ElderSign: 'eldersign',
  CurseToken: 'curse',
  BlessToken: 'bless',
  FrostToken: 'frost',
  BloodToken: 'blood',
}

/** The single source of truth for chaos token art, homebrew faces included. */
export function chaosTokenImage(face: TokenFace): string {
  const name = tokenImageNames[face as typeof tokenOrder[number]]
  if (name) return imgsrc(`chaos-tokens/ct_${name}.png`)

  if (face.includes(':')) {
    const [, campaign, key] = face.split(':')
    if (campaign && key) return imgsrc(`homebrew/${campaign}/chaos-tokens/${key}.png`)
  }

  return imgsrc("chaos-tokens/ct_blank.png")
}

import * as JsonDecoder from 'ts.data.json';
import { v2Optional, withDefault } from '@/arkham/parser';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Card, cardDecoder} from '@/arkham/types/Card';
import { SkillType, skillTypeDecoder} from '@/arkham/types/SkillType';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { TokenFace, tokenFaceDecoder } from '@/arkham/types/ChaosToken';

export type SkillTestStep
  = "DetermineSkillOfTestStep"
  | "SkillTestFastWindow1"
  | "CommitCardsFromHandToSkillTestStep"
  | "SkillTestFastWindow2"
  | "RevealChaosTokenStep"
  | "ResolveChaosSymbolEffectsStep"
  | "DetermineInvestigatorsModifiedSkillValueStep"
  | "DetermineSuccessOrFailureOfSkillTestStep"
  | "ApplySkillTestResultsStep"
  | "SkillTestEndsStep"

export type Source = {
  tag: string;
  contents: any; // eslint-disable-line
}

export const sourceDecoder = JsonDecoder.object<Source>({
  tag: JsonDecoder.string(),
  contents: JsonDecoder.succeed(),
}, 'Source');

type SkillTestBaseValue
  = { tag: 'SkillBaseValue' }
  | { tag: 'AndSkillBaseValue' }
  | { tag: 'HalfResourcesOf' }
  | { tag: 'FixedBaseValue' }

const baseValueDecoder = JsonDecoder.oneOf<SkillTestBaseValue>(
  [
    JsonDecoder.object({ tag: JsonDecoder.literal('SkillBaseValue') }, 'SkillBaseValue'),
    JsonDecoder.object({ tag: JsonDecoder.literal('AndSkillBaseValue') }, 'AndSkillBaseValue'),
    JsonDecoder.object({ tag: JsonDecoder.literal('HalfResourcesOf') }, 'HalfResourcesOf'),
    JsonDecoder.object({ tag: JsonDecoder.literal('FixedBaseValue') }, 'FixedBaseValue'),
  ],
  'SkillTestBaseValue',
);

/**
 * One card acting on one chaos token face, attributed to that card.
 *
 * `applied` means the engine has the modifier already, so the value is part of the
 * entry's `value`; otherwise the card declared the effect ahead of the reveal and the
 * value is a prediction (also already folded into `value`).
 */
export type ChaosTokenFaceEffect = {
  name: string | null
  cardCode: string | null
  value: number | null
  /** the prose half; may be an i18n key, so `t()` it and fall back to the raw string */
  text: string | null
  applied: boolean
}

export const chaosTokenFaceEffectDecoder = JsonDecoder.object<ChaosTokenFaceEffect>({
  name: JsonDecoder.nullable(JsonDecoder.string()),
  cardCode: JsonDecoder.nullable(JsonDecoder.string()),
  value: JsonDecoder.nullable(JsonDecoder.number()),
  text: JsonDecoder.nullable(JsonDecoder.string()),
  applied: JsonDecoder.boolean(),
}, 'ChaosTokenFaceEffect')

/**
 * What one card does to a face, as one string: "-1 If you fail, discard a card...".
 *
 * `translate` is vue-i18n's `t`. The backend may hand `text` over either as an i18n
 * key or as literal prose; keys have no spaces, so only those are worth asking about,
 * and `t` returns the key itself when there is no entry, which is the raw text anyway.
 */
export function chaosTokenEffectParts(
  effect: ChaosTokenFaceEffect,
  translate: (key: string) => string,
): { value: string | null, text: string | null } {
  return {
    // `null` means the card's number is only known once it resolves, so there is
    // nothing to show. A real 0 IS shown: a rule that currently contributes
    // nothing has to be distinguishable from one that contributes what it reads,
    // or the listed effects do not add up to the token's value.
    value: effect.value === null
      ? null
      : (effect.value > 0 ? `+${effect.value}` : `${effect.value}`),
    // The backend may hand `text` over either as an i18n key or as literal prose;
    // keys have no spaces, so only those are worth asking about, and `t` returns
    // the key itself when there is no entry, which is the raw text anyway.
    text: effect.text
      ? (/\s/.test(effect.text) ? effect.text : translate(effect.text))
      : null,
  }
}

/** The same, flattened to one string, for surfaces that can only show text. */
export function chaosTokenEffectDetail(
  effect: ChaosTokenFaceEffect,
  translate: (key: string) => string,
): string {
  const { value, text } = chaosTokenEffectParts(effect, translate)
  return [value, text].filter((p) => p !== null).join(' — ')
}

/** One distinct face in the chaos bag and what it is worth right now. */
export type ChaosTokenValueEntry = {
  face: TokenFace
  count: number
  /** null when the face auto fails or auto succeeds */
  value: number | null
  autoFail: boolean
  autoSuccess: boolean
  /** bless/curse/frost draw another token when revealed */
  revealsAnother: boolean
  /** absent on a game in flight under an older server, hence the default */
  effects: ChaosTokenFaceEffect[]
}

export const chaosTokenValueEntryDecoder = JsonDecoder.object<ChaosTokenValueEntry>({
  face: tokenFaceDecoder,
  count: JsonDecoder.number(),
  value: JsonDecoder.nullable(JsonDecoder.number()),
  autoFail: JsonDecoder.boolean(),
  autoSuccess: JsonDecoder.boolean(),
  revealsAnother: JsonDecoder.boolean(),
  effects: withDefault<ChaosTokenFaceEffect[]>([], JsonDecoder.array(chaosTokenFaceEffectDecoder, 'ChaosTokenFaceEffect[]')),
}, 'ChaosTokenValueEntry')

/**
 * Everything needed to answer "would the test succeed if the next token were X"
 * without reimplementing the engine's success rule.
 */
export type SkillTestValueBreakdown = {
  tokens: ChaosTokenValueEntry[]
  /** as it stands before the next token is revealed; includes committed icons */
  skillValue: number
  difficulty: number
  failTies: boolean
  autoFailIfSucceedByAtLeast: number[]
}

export const skillTestValueBreakdownDecoder = JsonDecoder.object<SkillTestValueBreakdown>({
  tokens: JsonDecoder.array(chaosTokenValueEntryDecoder, 'ChaosTokenValueEntry[]'),
  skillValue: JsonDecoder.number(),
  difficulty: JsonDecoder.number(),
  failTies: JsonDecoder.boolean(),
  autoFailIfSucceedByAtLeast: JsonDecoder.array(JsonDecoder.number(), 'number[]'),
}, 'SkillTestValueBreakdown')

/** How many chaos tokens the test reveals, and how many of those resolve. */
export type RevealStrategy
  = { tag: 'Reveal', contents: number }
  | { tag: 'RevealAndChoose', contents: [number, number] }
  | { tag: 'MultiReveal', contents: [RevealStrategy, RevealStrategy] }

export const revealStrategyDecoder: JsonDecoder.Decoder<RevealStrategy> = JsonDecoder.oneOf<RevealStrategy>(
  [
    JsonDecoder.object({
      tag: JsonDecoder.literal('Reveal'),
      contents: JsonDecoder.number(),
    }, 'Reveal'),
    JsonDecoder.object({
      tag: JsonDecoder.literal('RevealAndChoose'),
      contents: JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.number()], '[number, number]'),
    }, 'RevealAndChoose'),
    JsonDecoder.object({
      tag: JsonDecoder.literal('MultiReveal'),
      contents: JsonDecoder.tuple(
        [JsonDecoder.lazy(() => revealStrategyDecoder), JsonDecoder.lazy(() => revealStrategyDecoder)],
        '[RevealStrategy, RevealStrategy]',
      ),
    }, 'MultiReveal'),
  ],
  'RevealStrategy',
)

/** `Reveal 2 -> 1`, `2 + 1`, ... as a one-line label. */
export function describeRevealStrategy(strategy: RevealStrategy): string {
  switch (strategy.tag) {
    case 'Reveal': return `${strategy.contents}`
    case 'RevealAndChoose': return `${strategy.contents[0]} \u2192 ${strategy.contents[1]}`
    case 'MultiReveal':
      return `${describeRevealStrategy(strategy.contents[0])} + ${describeRevealStrategy(strategy.contents[1])}`
  }
}

export type SkillTest = {
  investigator: string;
  setAsideChaosTokens: ChaosToken[];
  revealedChaosTokens: ChaosToken[];
  resolvedChaosTokens: ChaosToken[];
  // result: SkillTestResult;
  committedCards: Card[]
  source: Source;
  target: Target;
  id: string
  action: string | null;
  targetCard?: string | null;
  sourceCard?: string | null;
  modifiedSkillValue: number;
  modifiedDifficulty: number;
  skills: SkillType[];
  step: SkillTestStep;
  baseValue: SkillTestBaseValue;
  result: null | { tag: string; contents?: [string, number] };
  resultForced: boolean;
  modifiers?: Modifier[];
  valueBreakdown?: SkillTestValueBreakdown;
  revealStrategy?: RevealStrategy;
}

export type SkillTestResults = {
  skillTestResultsSkillValue: number;
  skillTestResultsIconValue: number;
  skillTestResultsChaosTokensValue: number;
  skillTestResultsDifficulty: number;
  skillTestResultsResultModifiers: number | null;
  skillTestResultsSuccess: boolean;
}

const skillTestStepDecoder = JsonDecoder.oneOf<SkillTestStep>(
  [
    JsonDecoder.literal('DetermineSkillOfTestStep'),
    JsonDecoder.literal('SkillTestFastWindow1'),
    JsonDecoder.literal('CommitCardsFromHandToSkillTestStep'),
    JsonDecoder.literal('SkillTestFastWindow2'),
    JsonDecoder.literal('RevealChaosTokenStep'),
    JsonDecoder.literal('ResolveChaosSymbolEffectsStep'),
    JsonDecoder.literal('DetermineInvestigatorsModifiedSkillValueStep'),
    JsonDecoder.literal('DetermineSuccessOrFailureOfSkillTestStep'),
    JsonDecoder.literal('ApplySkillTestResultsStep'),
    JsonDecoder.literal('SkillTestEndsStep'),
  ],
  'SkillTestStep',
);

export const skillTestDecoder = JsonDecoder.object<SkillTest>(
  {
    investigator: JsonDecoder.string(),
    id: JsonDecoder.string(),
    action: JsonDecoder.nullable(JsonDecoder.string()),
    modifiedDifficulty: JsonDecoder.number(),
    setAsideChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    revealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    resolvedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    // result: skillTestResultDecoder,
    committedCards: JsonDecoder.record(JsonDecoder.array(cardDecoder, 'Card[]'), 'Record<string, Card[]>').map((record) => Object.values(record).flat()),
    source: sourceDecoder,
    target: targetDecoder,
    targetCard: v2Optional(JsonDecoder.string()),
    sourceCard: v2Optional(JsonDecoder.string()),
    modifiedSkillValue: JsonDecoder.number(),
    skills: JsonDecoder.array(skillTypeDecoder, 'SkillType[]'),
    step: JsonDecoder.fallback("DetermineSkillOfTestStep", skillTestStepDecoder),
    baseValue: baseValueDecoder,
    result: JsonDecoder.nullable(JsonDecoder.object({
      tag: JsonDecoder.string(),
      contents: v2Optional(JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.number()], '[string, number]')),
    }, 'SkillTestResult')),
    resultForced: JsonDecoder.fallback(false, JsonDecoder.boolean()),
    modifiers: v2Optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
    valueBreakdown: v2Optional(skillTestValueBreakdownDecoder),
    revealStrategy: v2Optional(revealStrategyDecoder),
  },
  'SkillTest',
);

export const skillTestResultsDecoder = JsonDecoder.object<SkillTestResults>(
  {
    skillTestResultsSkillValue: JsonDecoder.number(),
    skillTestResultsIconValue: JsonDecoder.number(),
    skillTestResultsChaosTokensValue: JsonDecoder.number(),
    skillTestResultsDifficulty: JsonDecoder.number(),
    skillTestResultsResultModifiers: JsonDecoder.nullable(JsonDecoder.number()),
    skillTestResultsSuccess: JsonDecoder.boolean(),
  },
  'SkillTestResults',
);

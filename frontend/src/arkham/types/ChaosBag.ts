import * as JsonDecoder from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export type ChaosBagStepState = Resolved | Decided | Undecided | Deciding

export type Resolved = {
  tag: "Resolved"
  tokens: ChaosToken[]
}

export const resolvedDecoder = JsonDecoder.object<Resolved>({
  tag: JsonDecoder.literal("Resolved"),
  tokens: JsonDecoder.array(chaosTokenDecoder, "Token[]")
}, 'Resolved')

export type Decided = {
  tag: "Decided"
  step: ChaosBagStep
}

export const decidedDecoder = JsonDecoder.object<Decided>({
  tag: JsonDecoder.literal("Decided"),
  step: JsonDecoder.lazy(() => chaosBagStepDecoder)
}, 'Decided')

export type Undecided = {
  tag: "Undecided"
  step: ChaosBagStep
}

export const undecidedDecoder = JsonDecoder.object<Undecided>({
  tag: JsonDecoder.literal("Undecided"),
  step: JsonDecoder.lazy(() => chaosBagStepDecoder)
}, 'Undecided')

export type Deciding = {
  tag: "Deciding"
  step: ChaosBagStep
}

export const decidingDecoder = JsonDecoder.object<Deciding>({
  tag: JsonDecoder.literal("Deciding"),
  step: JsonDecoder.lazy(() => chaosBagStepDecoder)
}, 'Deciding')

export const chaosBagStepStateDecoder = JsonDecoder.oneOf<ChaosBagStepState>([
  resolvedDecoder,
  decidedDecoder,
  undecidedDecoder,
  decidingDecoder,
], 'ChaosBagStepState')

export type ChaosBagStep = Draw | Choose | ChooseMatch | ChooseMatchChoice | Deciding

export type TokenStrategy = "IgnoreChoice" | "CancelChoice" | "ResolveChoice"

export type Draw = {
  tag: "Draw"
}

export const drawDecoder = JsonDecoder.object<Draw>({
  tag: JsonDecoder.literal("Draw")
}, 'Draw')

export const tokenStrategyDecoder = JsonDecoder.oneOf<TokenStrategy>([
  JsonDecoder.literal("IgnoreChoice"),
  JsonDecoder.literal("CancelChoice"),
  JsonDecoder.literal("ResolveChoice"),
], 'TokenStrategy')

export type Choose = {
  tag: "Choose"
  tokenStrategy: TokenStrategy
  amount: number
  steps: ChaosBagStepState[]
  tokenGroups: ChaosToken[][]
}

export const chooseDecoder = JsonDecoder.object<Choose>({
  tag: JsonDecoder.literal("Choose"),
  tokenStrategy: tokenStrategyDecoder,
  amount: JsonDecoder.number(),
  steps: JsonDecoder.array(chaosBagStepStateDecoder, 'ChaosBagStepState[]'),
  tokenGroups: JsonDecoder.array(JsonDecoder.array(chaosTokenDecoder, 'Token[]'), 'Token[][]'),
}, 'Choose')

export type ChooseMatch = {
  tag: "ChooseMatch"
  tokenStrategy: TokenStrategy
  amount: number
  steps: ChaosBagStepState[]
  tokenGroups: ChaosToken[][]
  // tokenMatcher
}

export const chooseMatchDecoder = JsonDecoder.object<ChooseMatch>({
  tag: JsonDecoder.literal("ChooseMatch"),
  tokenStrategy: tokenStrategyDecoder,
  amount: JsonDecoder.number(),
  steps: JsonDecoder.array(chaosBagStepStateDecoder, 'ChaosBagStepState[]'),
  tokenGroups: JsonDecoder.array(JsonDecoder.array(chaosTokenDecoder, 'Token[]'), 'Token[][]'),
}, 'ChooseMatch')

export type ChooseMatchChoice = {
  tag: "ChooseMatchChoice"
  steps: ChaosBagStepState[]
  tokenGroups: ChaosToken[][]
  // tokenMatcher
}

export const chooseMatchChoiceDecoder = JsonDecoder.object<ChooseMatchChoice>({
  tag: JsonDecoder.literal("ChooseMatchChoice"),
  steps: JsonDecoder.array(chaosBagStepStateDecoder, 'ChaosBagStepState[]'),
  tokenGroups: JsonDecoder.array(JsonDecoder.array(chaosTokenDecoder, 'Token[]'), 'Token[][]'),
}, 'ChooseMatch')

export const chaosBagStepDecoder: JsonDecoder.Decoder<ChaosBagStep> = JsonDecoder.oneOf<ChaosBagStep>([
  drawDecoder,
  chooseDecoder,
  chooseMatchDecoder,
  chooseMatchChoiceDecoder,
  decidingDecoder,
], 'ChaosBagStep')


export type ChaosBag = {
  chaosTokens: ChaosToken[]
  choice: ChaosBagStepState | null
}

export const chaosBagDecoder = JsonDecoder.object<ChaosBag>({
  chaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  choice: JsonDecoder.nullable(chaosBagStepStateDecoder)
}, 'ChaosBag');

import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export type ChaosBagStepState = Resolved | Decided | Undecided | Deciding

export interface Resolved {
  tag: "Resolved"
  tokens: ChaosToken[]
}

export const resolvedDecoder = JsonDecoder.object<Resolved>({
  tag: JsonDecoder.isExactly("Resolved"),
  tokens: JsonDecoder.array(chaosTokenDecoder, "Token[]")
}, 'Resolved')

export interface Decided {
  tag: "Decided"
  step: ChaosBagStep
}

export const decidedDecoder = JsonDecoder.object<Decided>({
  tag: JsonDecoder.isExactly("Decided"),
  step: JsonDecoder.lazy(() => chaosBagStepDecoder)
}, 'Decided')

export interface Undecided {
  tag: "Undecided"
  step: ChaosBagStep
}

export const undecidedDecoder = JsonDecoder.object<Undecided>({
  tag: JsonDecoder.isExactly("Undecided"),
  step: JsonDecoder.lazy(() => chaosBagStepDecoder)
}, 'Undecided')

export interface Deciding {
  tag: "Deciding"
  step: ChaosBagStep
}

export const decidingDecoder = JsonDecoder.object<Deciding>({
  tag: JsonDecoder.isExactly("Deciding"),
  step: JsonDecoder.lazy(() => chaosBagStepDecoder)
}, 'Deciding')

export const chaosBagStepStateDecoder = JsonDecoder.oneOf<ChaosBagStepState>([
  resolvedDecoder,
  decidedDecoder,
  undecidedDecoder,
  decidingDecoder,
], 'ChaosBagStepState')

export type ChaosBagStep = Draw | Choose | ChooseMatch | ChooseMatchChoice

export type TokenStrategy = "IgnoreChoice" | "CancelChoice" | "ResolveChoice"

export interface Draw {
  tag: "Draw"
}

export const drawDecoder = JsonDecoder.object<Draw>({
  tag: JsonDecoder.isExactly("Draw")
}, 'Draw')

export const tokenStrategyDecoder = JsonDecoder.oneOf<TokenStrategy>([
  JsonDecoder.isExactly("IgnoreChoice"),
  JsonDecoder.isExactly("CancelChoice"),
  JsonDecoder.isExactly("ResolveChoice"),
], 'TokenStrategy')

export interface Choose {
  tag: "Choose"
  tokenStrategy: TokenStrategy
  amount: number
  steps: ChaosBagStepState[]
  tokenGroups: ChaosToken[][]
}

export const chooseDecoder = JsonDecoder.object<Choose>({
  tag: JsonDecoder.isExactly("Choose"),
  tokenStrategy: tokenStrategyDecoder,
  amount: JsonDecoder.number,
  steps: JsonDecoder.array(chaosBagStepStateDecoder, 'ChaosBagStepState[]'),
  tokenGroups: JsonDecoder.array(JsonDecoder.array(chaosTokenDecoder, 'Token[]'), 'Token[][]'),
}, 'Choose')

export interface ChooseMatch {
  tag: "ChooseMatch"
  tokenStrategy: TokenStrategy
  amount: number
  steps: ChaosBagStepState[]
  tokenGroups: ChaosToken[][]
  // tokenMatcher
}

export const chooseMatchDecoder = JsonDecoder.object<ChooseMatch>({
  tag: JsonDecoder.isExactly("ChooseMatch"),
  tokenStrategy: tokenStrategyDecoder,
  amount: JsonDecoder.number,
  steps: JsonDecoder.array(chaosBagStepStateDecoder, 'ChaosBagStepState[]'),
  tokenGroups: JsonDecoder.array(JsonDecoder.array(chaosTokenDecoder, 'Token[]'), 'Token[][]'),
}, 'ChooseMatch')

export interface ChooseMatchChoice {
  tag: "ChooseMatchChoice"
  steps: ChaosBagStepState[]
  tokenGroups: ChaosToken[][]
  // tokenMatcher
}

export const chooseMatchChoiceDecoder = JsonDecoder.object<ChooseMatchChoice>({
  tag: JsonDecoder.isExactly("ChooseMatchChoice"),
  steps: JsonDecoder.array(chaosBagStepStateDecoder, 'ChaosBagStepState[]'),
  tokenGroups: JsonDecoder.array(JsonDecoder.array(chaosTokenDecoder, 'Token[]'), 'Token[][]'),
}, 'ChooseMatch')

export const chaosBagStepDecoder: JsonDecoder.Decoder<ChaosBagStep> = JsonDecoder.oneOf<ChaosBagStep>([
  drawDecoder,
  chooseDecoder,
  chooseMatchDecoder,
  chooseMatchChoiceDecoder,
], 'ChaosBagStep')


export interface ChaosBag {
  tokens: ChaosToken[]
  choice: ChaosBagStepState | null
}

export const chaosBagDecoder = JsonDecoder.object<ChaosBag>({
  tokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  choice: JsonDecoder.nullable(chaosBagStepStateDecoder)
}, 'ChaosBag');


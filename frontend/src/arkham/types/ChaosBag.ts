import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

type ChaosBagStep = { "tag": "Draw" } | { "tag": "ChooseMatch", "contents": [number, ChaosBagStep[], ] }
type ChaosBagChoice = { "tag": "Decided", "contents": ChaosBagStep } | { "tag": "Undecided", "contents": ChaosBagStep }

export interface ChaosBag {
  tokens: ChaosToken[]
  choice: ChaosBagChoice | null
}

export const chaosBagStepDecoder: JsonDecoder.Decoder<ChaosBagStep> = JsonDecoder.oneOf<ChaosBagStep>([
  JsonDecoder.object<ChaosBagStep>({
    tag: JsonDecoder.isExactly("Draw"),
  }, 'ChaosBagStep'),
  JsonDecoder.object<ChaosBagStep>({
    tag: JsonDecoder.isExactly("ChooseMatch"),
    contents: JsonDecoder.tuple([JsonDecoder.number, JsonDecoder.lazy<ChaosBagStep[]>(() => JsonDecoder.array(chaosBagStepDecoder, 'ChaosBagStep[]')), JsonDecoder.succeed, JsonDecoder.succeed], '[number, ChaosBagStep, any, any]').map(([n, ss]) => [n, ss])
  }, 'ChaosBagStep'),
], 'ChaosBagStep');

export const chaosBagChoiceDecoder = JsonDecoder.oneOf([
  JsonDecoder.object<ChaosBagChoice>({
    tag: JsonDecoder.isExactly("Decided"),
    contents: chaosBagStepDecoder
  }, 'ChaosBagChoice'),
  JsonDecoder.object<ChaosBagChoice>({
    tag: JsonDecoder.isExactly("Undecided"),
    contents: chaosBagStepDecoder
  }, 'ChaosBagChoice')
], 'ChaosBagChoice')

export const chaosBagDecoder = JsonDecoder.object<ChaosBag>({
  tokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  choice: JsonDecoder.nullable(chaosBagChoiceDecoder)
}, 'ChaosBag');


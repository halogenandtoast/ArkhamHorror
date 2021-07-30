import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export interface ChaosBag {
  tokens: ChaosToken[]
}

export const chaosBagDecoder = JsonDecoder.object<ChaosBag>({
  tokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]')
}, 'ChaosBag');


import { JsonDecoder } from 'ts.data.json';
import { Customization, customizationsDecoder } from '@/arkham/types/Customization';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export type Skill = {
  id: number;
  cardId: number;
  cardCode: string;
  customizations: Customization[];
  sealedChaosTokens: ChaosToken[];
}

export const skillDecoder = JsonDecoder.object<Skill>({
  id: JsonDecoder.number,
  cardId: JsonDecoder.number,
  cardCode: JsonDecoder.string,
  customizations: customizationsDecoder,
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
}, 'Skill');

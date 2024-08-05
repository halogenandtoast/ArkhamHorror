import { JsonDecoder } from 'ts.data.json';
import { Customization, customizationsDecoder } from '@/arkham/types/Customization';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export type Skill = {
  id: string;
  cardId: string;
  cardCode: string;
  customizations: Customization[];
  sealedChaosTokens: ChaosToken[];
}

export const skillDecoder = JsonDecoder.object<Skill>({
  id: JsonDecoder.string,
  cardId: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  customizations: customizationsDecoder,
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
}, 'Skill');

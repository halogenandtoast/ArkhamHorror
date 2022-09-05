import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export interface Enemy {
  id: string;
  cardCode: string;
  damage: number;
  doom: number;
  clues: number;
  resources: number;
  exhausted: boolean;
  engagedInvestigators: string[];
  treacheries: string[];
  assets: string[];
  asSelfLocation: string | null;
  sealedTokens: ChaosToken[];
}

export const enemyDecoder = JsonDecoder.object<Enemy>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  damage: JsonDecoder.number,
  doom: JsonDecoder.number,
  clues: JsonDecoder.number,
  resources: JsonDecoder.number,
  exhausted: JsonDecoder.boolean,
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorIds[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  asSelfLocation: JsonDecoder.nullable(JsonDecoder.string),
  sealedTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]')
}, 'Enemy');

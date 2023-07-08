import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import {
  Card,
  cardDecoder,
} from '@/arkham/types/Card';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';

export interface Uses {
  amount: number; // eslint-disable-line
}

export const usesDecoder = JsonDecoder.object<Uses>({
  amount: JsonDecoder.number,
}, 'Uses');

export interface Asset {
  id: string;
  cardCode: string;
  cardId: string;
  owner: string | null;
  health: number | null;
  damage: number;
  sanity: number | null;
  clues: number;
  resources: number;
  uses: Uses | null;
  exhausted: boolean;
  horror: number;
  doom: number;
  events: string[];
  assets: string[];
  cardsUnderneath: Card[];
  sealedTokens: ChaosToken[];
  keys: ArkhamKey[];
}

export const assetDecoder = JsonDecoder.object<Asset>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  cardId: JsonDecoder.string,
  owner: JsonDecoder.nullable(JsonDecoder.string),
  health: JsonDecoder.nullable(JsonDecoder.number),
  damage: JsonDecoder.number,
  sanity: JsonDecoder.nullable(JsonDecoder.number),
  clues: JsonDecoder.number,
  resources: JsonDecoder.number,
  uses: JsonDecoder.nullable(usesDecoder),
  exhausted: JsonDecoder.boolean,
  horror: JsonDecoder.number,
  doom: JsonDecoder.number,
  events: JsonDecoder.array<string>(JsonDecoder.string, 'EventId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  sealedTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
}, 'Asset');

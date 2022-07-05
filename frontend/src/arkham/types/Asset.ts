import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import {
  Card,
  cardDecoder,
} from '@/arkham/types/Card';

export interface Uses {
  amount: number; // eslint-disable-line
}

export const usesDecoder = JsonDecoder.object<Uses>({
  amount: JsonDecoder.number,
}, 'Uses');

export interface AssetContents {
  id: string;
  cardCode: string;
  owner: string | null;
  health: number | null;
  damage: number;
  sanity: number | null;
  clues: number;
  uses: Uses | null;
  exhausted: boolean;
  horror: number;
  doom: number;
  cardsUnderneath: Card[];
  sealedTokens: ChaosToken[];
}

export const assetContentsDecoder = JsonDecoder.object<AssetContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  owner: JsonDecoder.nullable(JsonDecoder.string),
  health: JsonDecoder.nullable(JsonDecoder.number),
  damage: JsonDecoder.number,
  sanity: JsonDecoder.nullable(JsonDecoder.number),
  clues: JsonDecoder.number,
  uses: JsonDecoder.nullable(usesDecoder),
  exhausted: JsonDecoder.boolean,
  horror: JsonDecoder.number,
  doom: JsonDecoder.number,
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  sealedTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]')
}, 'AssetContents');

export interface Asset {
  tag: string;
  contents: AssetContents;
}

export const assetDecoder = JsonDecoder.object<Asset>({
  tag: JsonDecoder.string,
  contents: assetContentsDecoder,
}, 'Asset');

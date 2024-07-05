import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import {
  Card,
  cardDecoder,
} from '@/arkham/types/Card';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';

export type Asset = {
  id: string;
  cardCode: string;
  cardId: string;
  owner: string | null;
  health: number | null;
  sanity: number | null;
  tokens: Tokens;
  exhausted: boolean;
  permanent: boolean;
  events: string[];
  treacheries: string[];
  assets: string[];
  cardsUnderneath: Card[];
  sealedChaosTokens: ChaosToken[];
  keys: ArkhamKey[];
  customizations: [number, number][];
}

export const assetDecoder = JsonDecoder.object<Asset>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  cardId: JsonDecoder.string,
  owner: JsonDecoder.nullable(JsonDecoder.string),
  health: JsonDecoder.nullable(JsonDecoder.number),
  tokens: tokensDecoder,
  sanity: JsonDecoder.nullable(JsonDecoder.number),
  exhausted: JsonDecoder.boolean,
  permanent: JsonDecoder.boolean,
  events: JsonDecoder.array<string>(JsonDecoder.string, 'EventId[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  customizations: JsonDecoder.array<[number, number]>(JsonDecoder.tuple([JsonDecoder.number, JsonDecoder.number], 'Customization'), 'Customization[]'),
}, 'Asset');

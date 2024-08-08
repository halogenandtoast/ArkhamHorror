import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import {
  Card,
  cardDecoder,
} from '@/arkham/types/Card';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Customization, customizationsDecoder } from '@/arkham/types/Customization';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

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
  flipped: boolean;
  events: string[];
  treacheries: string[];
  enemies: string[];
  assets: string[];
  cardsUnderneath: Card[];
  sealedChaosTokens: ChaosToken[];
  keys: ArkhamKey[];
  customizations: Customization[];
  marketDeck?: Card[]
  spiritDeck?: Card[]
  modifiers?: Modifier[];
  mutated?: string;
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
  flipped: JsonDecoder.boolean,
  events: JsonDecoder.array<string>(JsonDecoder.string, 'EventId[]'),
  enemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  customizations: customizationsDecoder,
  marketDeck: JsonDecoder.optional(JsonDecoder.array<Card>(cardDecoder, 'Card[]')),
  spiritDeck: JsonDecoder.optional(JsonDecoder.array<Card>(cardDecoder, 'Card[]')),
  modifiers: JsonDecoder.optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
  mutated: JsonDecoder.optional(JsonDecoder.string),
}, 'Asset');

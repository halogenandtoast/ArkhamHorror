import { JsonDecoder } from 'ts.data.json';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';

export interface EnemyContents {
  id: string;
  cardDef: CardDef;
  damage: number;
  doom: number;
  engagedInvestigators: string[];
  treacheries: string[];
  assets: string[];
}

export const enemyContentsDecoder = JsonDecoder.object<EnemyContents>({
  id: JsonDecoder.string,
  cardDef: cardDefDecoder,
  damage: JsonDecoder.number,
  doom: JsonDecoder.number,
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorIds[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
}, 'EnemyContents');

export interface Enemy {
  tag: string;
  contents: EnemyContents;
}

export const enemyDecoder = JsonDecoder.object<Enemy>({
  tag: JsonDecoder.string,
  contents: enemyContentsDecoder,
}, 'Enemy');

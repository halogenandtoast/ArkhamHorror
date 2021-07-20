import { JsonDecoder } from 'ts.data.json';

export interface EnemyContents {
  id: string;
  cardCode: string;
  damage: number;
  doom: number;
  exhausted: boolean;
  engagedInvestigators: string[];
  treacheries: string[];
  assets: string[];
  asSelfLocation: string | null;
}

export const enemyContentsDecoder = JsonDecoder.object<EnemyContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  damage: JsonDecoder.number,
  doom: JsonDecoder.number,
  exhausted: JsonDecoder.boolean,
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorIds[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  asSelfLocation: JsonDecoder.nullable(JsonDecoder.string),
}, 'EnemyContents');

export interface Enemy {
  tag: string;
  contents: EnemyContents;
}

export const enemyDecoder = JsonDecoder.object<Enemy>({
  tag: JsonDecoder.string,
  contents: enemyContentsDecoder,
}, 'Enemy');

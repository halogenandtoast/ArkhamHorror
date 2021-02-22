import { JsonDecoder } from 'ts.data.json';

export interface EnemyName {
  title: string;
  subtitle: string | null;
}

export const enemyNameDecoder = JsonDecoder.object<EnemyName>(
  {
    title: JsonDecoder.string,
    subtitle: JsonDecoder.nullable(JsonDecoder.string),
  },
  'EnemyName'
);

export interface EnemyContents {
  id: string;
  cardCode: string;
  name: EnemyName;
  damage: number;
  doom: number;
  engagedInvestigators: string[];
  treacheries: string[];
  assets: string[];
}

export const enemyContentsDecoder = JsonDecoder.object<EnemyContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  name: enemyNameDecoder,
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

import { JsonDecoder } from 'ts.data.json';

export interface EnemyContents {
  id: string;
  cardCode: string;
  name: string;
  damage: number;
  doom: number;
  engagedInvestigators: string[];
  treacheries: string[];
}

export const enemyContentsDecoder = JsonDecoder.object<EnemyContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  name: JsonDecoder.string,
  damage: JsonDecoder.number,
  doom: JsonDecoder.number,
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorIds[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
}, 'EnemyContents');

export interface Enemy {
  tag: string;
  contents: EnemyContents;
}

export const enemyDecoder = JsonDecoder.object<Enemy>({
  tag: JsonDecoder.string,
  contents: enemyContentsDecoder,
}, 'Enemy');

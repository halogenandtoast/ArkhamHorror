import { JsonDecoder } from 'ts.data.json';

export interface EnemyContents {
  id: string;
  cardCode: string;
  name: string;
  damage: number;
  engagedInvestigators: string[];
}

export const enemyContentsDecoder = JsonDecoder.object<EnemyContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  name: JsonDecoder.string,
  damage: JsonDecoder.number,
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorIds[]'),
}, 'EnemyContents');

export interface Enemy {
  tag: string;
  contents: EnemyContents;
}

export const enemyDecoder = JsonDecoder.object<Enemy>({
  tag: JsonDecoder.string,
  contents: enemyContentsDecoder,
}, 'Enemy');

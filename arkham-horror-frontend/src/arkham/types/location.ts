import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamInvestigator,
  arkhamInvestigatorDecoder,
  ArkhamEnemy,
  arkhamEnemyDecoder,
} from '@/arkham/types';

export type ArkhamLocationSymbol = 'Circle' | 'Heart';
export type ArkhamLocationStatus = 'Revealed' | 'Unrevealed' | 'OutOfPlay';

export const arkhamLocationSymbolDecoder = JsonDecoder.oneOf<ArkhamLocationSymbol>([
  JsonDecoder.isExactly('Circle'),
  JsonDecoder.isExactly('Heart'),
], 'ArkhamLocationSymbol');

export const arkhamLocationStatusDecoder = JsonDecoder.oneOf<ArkhamLocationStatus>([
  JsonDecoder.isExactly('Revealed'),
  JsonDecoder.isExactly('Unrevealed'),
  JsonDecoder.isExactly('OutOfPlay'),
], 'ArkhamLocationStatus');

export interface ArkhamLocation {
  name: string;
  cardCode: string;
  locationSymbol: ArkhamLocationSymbol | null;
  connectedLocationSymbols: ArkhamLocationSymbol[];
  shroud: number;
  image: string;
  investigators: ArkhamInvestigator[];
  enemies: ArkhamEnemy[];
  clues: number;
  doom: number;
  status: ArkhamLocationStatus;
}

export const arkhamLocationDecoder = JsonDecoder.object<ArkhamLocation>(
  {
    name: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    locationSymbol: JsonDecoder.nullable<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder),
    connectedLocationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    shroud: JsonDecoder.number,
    image: JsonDecoder.string,
    investigators: JsonDecoder.array<ArkhamInvestigator>(arkhamInvestigatorDecoder, 'ArkhamInvestigator[]'),
    enemies: JsonDecoder.array<ArkhamEnemy>(arkhamEnemyDecoder, 'ArkhamEnemy[]'),
    clues: JsonDecoder.number,
    doom: JsonDecoder.number,
    status: arkhamLocationStatusDecoder,
  },
  'ArkhamLocation',
);

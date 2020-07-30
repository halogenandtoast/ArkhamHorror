import { JsonDecoder } from 'ts.data.json';

export type Phase = 'InvestigationPhase' | 'EnemyPhase' | 'UpkeepPhase' | 'MythosPhase';

export const phaseDecoder = JsonDecoder.oneOf<Phase>([
  JsonDecoder.isExactly('InvestigationPhase'),
  JsonDecoder.isExactly('EnemyPhase'),
  JsonDecoder.isExactly('UpkeepPhase'),
  JsonDecoder.isExactly('MythosPhase'),
], 'Phase');

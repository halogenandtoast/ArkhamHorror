import { JsonDecoder } from 'ts.data.json';

export type Phase = 'InvestigationPhase' | 'UpkeepPhase' | 'MythosPhase';

export const phaseDecoder = JsonDecoder.oneOf<Phase>([
  JsonDecoder.isExactly('InvestigationPhase'),
  JsonDecoder.isExactly('UpkeepPhase'),
  JsonDecoder.isExactly('MythosPhase'),
], 'Phase');

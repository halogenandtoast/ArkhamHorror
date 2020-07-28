import { JsonDecoder } from 'ts.data.json';

export type Phase = 'InvestigationPhase' | 'UpkeepPhase';

export const phaseDecoder = JsonDecoder.oneOf<Phase>([
  JsonDecoder.isExactly('InvestigationPhase'),
  JsonDecoder.isExactly('UpkeepPhase'),
], 'Phase');

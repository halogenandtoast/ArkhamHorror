import { JsonDecoder } from 'ts.data.json';

export type Phase = 'InvestigationPhase';

export const phaseDecoder = JsonDecoder.oneOf<Phase>([
  JsonDecoder.isExactly('InvestigationPhase'),
], 'Phase');

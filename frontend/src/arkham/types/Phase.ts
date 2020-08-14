import { JsonDecoder } from 'ts.data.json';

export type Phase = 'CampaignPhase' | 'InvestigationPhase' | 'EnemyPhase' | 'UpkeepPhase' | 'MythosPhase';

export const phaseDecoder = JsonDecoder.oneOf<Phase>([
  JsonDecoder.isExactly('CampaignPhase'),
  JsonDecoder.isExactly('InvestigationPhase'),
  JsonDecoder.isExactly('EnemyPhase'),
  JsonDecoder.isExactly('UpkeepPhase'),
  JsonDecoder.isExactly('MythosPhase'),
], 'Phase');

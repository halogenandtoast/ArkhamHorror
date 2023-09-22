import { JsonDecoder } from 'ts.data.json';

export type Phase = 'CampaignPhase' | 'InvestigationPhase' | 'EnemyPhase' | 'UpkeepPhase' | 'MythosPhase';

export const phaseDecoder = JsonDecoder.oneOf<Phase>([
  JsonDecoder.isExactly('CampaignPhase'),
  JsonDecoder.isExactly('InvestigationPhase'),
  JsonDecoder.isExactly('EnemyPhase'),
  JsonDecoder.isExactly('UpkeepPhase'),
  JsonDecoder.isExactly('MythosPhase'),
], 'Phase');

export type MythosPhaseStep
  = 'MythosPhaseBeginsStep'
  | 'PlaceDoomOnAgendaStep'
  | 'CheckDoomThresholdStep'
  | 'EachInvestigatorDrawsEncounterCardStep'
  | 'MythosPhaseWindow'
  | 'MythosPhaseEndsStep'

export type InvestigationPhaseStep
  = 'InvestigationPhaseBeginsStep'
  | 'InvestigationPhaseBeginsWindow'
  | 'NextInvestigatorsTurnBeginsStep'
  | 'NextInvestigatorsTurnBeginsWindow'
  | 'InvestigatorTakesActionStep'
  | 'InvestigatorsTurnEndsStep'
  | 'InvestigationPhaseEndsStep'

export type EnemyPhaseStep
  = 'EnemyPhaseBeginsStep'
  | 'HunterEnemiesMoveStep'
  | 'ResolveAttacksWindow'
  | 'ResolveAttacksStep'
  | 'AfterResolveAttacksWindow'
  | 'EnemyPhaseEndsStep'

export type UpkeepPhaseStep
  = 'UpkeepPhaseBeginsStep'
  | 'UpkeepPhaseBeginsWindow'
  | 'ResetActionsStep'
  | 'ReadyExhaustedStep'
  | 'DrawCardAndGainResourceStep'
  | 'CheckHandSizeStep'
  | 'UpkeepPhaseEndsStep'

export type PhaseStep = { tag: "MythosPhaseStep", contents: MythosPhaseStep }  | { tag: "InvestigationPhaseStep", contents: InvestigationPhaseStep }  | { tag: "EnemyPhaseStep", contents: EnemyPhaseStep }  | { tag: "UpkeepPhaseStep", contents: UpkeepPhaseStep }

const mythosPhaseStepDecoder = JsonDecoder.oneOf<MythosPhaseStep>([
  JsonDecoder.isExactly('MythosPhaseBeginsStep'),
  JsonDecoder.isExactly('PlaceDoomOnAgendaStep'),
  JsonDecoder.isExactly('CheckDoomThresholdStep'),
  JsonDecoder.isExactly('EachInvestigatorDrawsEncounterCardStep'),
  JsonDecoder.isExactly('MythosPhaseWindow'),
  JsonDecoder.isExactly('MythosPhaseEndsStep'),
], 'MythosPhaseStep');

const investigationPhaseStepDecoder = JsonDecoder.oneOf<InvestigationPhaseStep>([
  JsonDecoder.isExactly('InvestigationPhaseBeginsStep'),
  JsonDecoder.isExactly('InvestigationPhaseBeginsWindow'),
  JsonDecoder.isExactly('NextInvestigatorsTurnBeginsStep'),
  JsonDecoder.isExactly('NextInvestigatorsTurnBeginsWindow'),
  JsonDecoder.isExactly('InvestigatorTakesActionStep'),
  JsonDecoder.isExactly('InvestigatorsTurnEndsStep'),
  JsonDecoder.isExactly('InvestigationPhaseEndsStep'),
], 'InvestigationPhaseStep');

const enemyPhaseStepDecoder = JsonDecoder.oneOf<EnemyPhaseStep>([
  JsonDecoder.isExactly('EnemyPhaseBeginsStep'),
  JsonDecoder.isExactly('HunterEnemiesMoveStep'),
  JsonDecoder.isExactly('ResolveAttacksWindow'),
  JsonDecoder.isExactly('ResolveAttacksStep'),
  JsonDecoder.isExactly('AfterResolveAttacksWindow'),
  JsonDecoder.isExactly('EnemyPhaseEndsStep'),
], 'EnemyPhaseStep');

const upkeepPhaseStepDecoder = JsonDecoder.oneOf<UpkeepPhaseStep>([
  JsonDecoder.isExactly('UpkeepPhaseBeginsStep'),
  JsonDecoder.isExactly('UpkeepPhaseBeginsWindow'),
  JsonDecoder.isExactly('ResetActionsStep'),
  JsonDecoder.isExactly('ReadyExhaustedStep'),
  JsonDecoder.isExactly('DrawCardAndGainResourceStep'),
  JsonDecoder.isExactly('CheckHandSizeStep'),
  JsonDecoder.isExactly('UpkeepPhaseEndsStep'),
], 'UpkeepPhaseStep');

export const phaseStepDecoder = JsonDecoder.oneOf<PhaseStep>([
  JsonDecoder.object({ tag: JsonDecoder.isExactly('MythosPhaseStep'), contents: mythosPhaseStepDecoder }, 'MythosPhaseStep'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('InvestigationPhaseStep'), contents: investigationPhaseStepDecoder }, 'InvestigationPhaseStep'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('EnemyPhaseStep'), contents: enemyPhaseStepDecoder }, 'EnemyPhaseStep'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('UpkeepPhaseStep'), contents: upkeepPhaseStepDecoder }, 'UpkeepPhaseStep'),
], 'PhaseStep');

import * as JsonDecoder from 'ts.data.json';

export type Phase = 'CampaignPhase' | 'InvestigationPhase' | 'EnemyPhase' | 'UpkeepPhase' | 'MythosPhase';

export const phaseDecoder = JsonDecoder.oneOf<Phase>([
  JsonDecoder.literal('CampaignPhase'),
  JsonDecoder.literal('InvestigationPhase'),
  JsonDecoder.literal('EnemyPhase'),
  JsonDecoder.literal('UpkeepPhase'),
  JsonDecoder.literal('MythosPhase'),
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
  JsonDecoder.literal('MythosPhaseBeginsStep'),
  JsonDecoder.literal('PlaceDoomOnAgendaStep'),
  JsonDecoder.literal('CheckDoomThresholdStep'),
  JsonDecoder.literal('EachInvestigatorDrawsEncounterCardStep'),
  JsonDecoder.literal('MythosPhaseWindow'),
  JsonDecoder.literal('MythosPhaseEndsStep'),
], 'MythosPhaseStep');

const investigationPhaseStepDecoder = JsonDecoder.oneOf<InvestigationPhaseStep>([
  JsonDecoder.literal('InvestigationPhaseBeginsStep'),
  JsonDecoder.literal('InvestigationPhaseBeginsWindow'),
  JsonDecoder.literal('NextInvestigatorsTurnBeginsStep'),
  JsonDecoder.literal('NextInvestigatorsTurnBeginsWindow'),
  JsonDecoder.literal('InvestigatorTakesActionStep'),
  JsonDecoder.literal('InvestigatorsTurnEndsStep'),
  JsonDecoder.literal('InvestigationPhaseEndsStep'),
], 'InvestigationPhaseStep');

const enemyPhaseStepDecoder = JsonDecoder.oneOf<EnemyPhaseStep>([
  JsonDecoder.literal('EnemyPhaseBeginsStep'),
  JsonDecoder.literal('HunterEnemiesMoveStep'),
  JsonDecoder.literal('ResolveAttacksWindow'),
  JsonDecoder.literal('ResolveAttacksStep'),
  JsonDecoder.literal('AfterResolveAttacksWindow'),
  JsonDecoder.literal('EnemyPhaseEndsStep'),
], 'EnemyPhaseStep');

const upkeepPhaseStepDecoder = JsonDecoder.oneOf<UpkeepPhaseStep>([
  JsonDecoder.literal('UpkeepPhaseBeginsStep'),
  JsonDecoder.literal('UpkeepPhaseBeginsWindow'),
  JsonDecoder.literal('ResetActionsStep'),
  JsonDecoder.literal('ReadyExhaustedStep'),
  JsonDecoder.literal('DrawCardAndGainResourceStep'),
  JsonDecoder.literal('CheckHandSizeStep'),
  JsonDecoder.literal('UpkeepPhaseEndsStep'),
], 'UpkeepPhaseStep');

export const phaseStepDecoder = JsonDecoder.oneOf<PhaseStep>([
  JsonDecoder.object({ tag: JsonDecoder.literal('MythosPhaseStep'), contents: mythosPhaseStepDecoder }, 'MythosPhaseStep'),
  JsonDecoder.object({ tag: JsonDecoder.literal('InvestigationPhaseStep'), contents: investigationPhaseStepDecoder }, 'InvestigationPhaseStep'),
  JsonDecoder.object({ tag: JsonDecoder.literal('EnemyPhaseStep'), contents: enemyPhaseStepDecoder }, 'EnemyPhaseStep'),
  JsonDecoder.object({ tag: JsonDecoder.literal('UpkeepPhaseStep'), contents: upkeepPhaseStepDecoder }, 'UpkeepPhaseStep'),
], 'PhaseStep');

import * as JsonDecoder from 'ts.data.json'
import scenarios from '@/arkham/data/scenarios'
import type { Game } from '@/arkham/types/Game'
import { toCamelCase } from '@/arkham/helpers'
import { useI18n } from 'vue-i18n';

export type CampaignStep
  = PrologueStep
  | ScenarioStep
  | InterludeStep
  | InterludeStepPart
  | UpgradeDeckStep
  | EpilogueStep
  | ResupplyPoint
  | CheckpointStep
  | CampaignSpecificStep
  | ContinueCampaignStep
  | StandaloneScenarioStep

export type PrologueStep = {
  tag: 'PrologueStep'
}

export type StandaloneScenarioStep = {
  tag: 'StandaloneScenarioStep'
  contents: [string, any]
}

export type ResupplyPoint = {
  tag: 'ResupplyPoint';
}

export type CampaignSpecificStep = {
  tag: 'CampaignSpecificStep';
  contents: string;
}

export type EpilogueStep = {
  tag: 'EpilogueStep';
}

export type Continuation = {
  nextStep: CampaignStep;
  canUpgradeDecks: boolean;
}

export type ContinueCampaignStep = {
  tag: 'ContinueCampaignStep';
  contents: Continuation;
}

export const continuationDecoder = JsonDecoder.object<Continuation>(
  {
    nextStep: JsonDecoder.lazy<CampaignStep>(() => campaignStepDecoder),
    canUpgradeDecks: JsonDecoder.boolean(),
  },
  'Continuation',
);

export const prologueStepDecoder = JsonDecoder.object<PrologueStep>(
  {
    tag: JsonDecoder.literal('PrologueStep'),
  },
  'PrologueStep',
);

export const resupplyPointStepDecoder = JsonDecoder.object<ResupplyPoint>(
  {
    tag: JsonDecoder.literal('ResupplyPoint'),
  },
  'ResupplyPoint',
);

export const campaignSpecificStepDecoder = JsonDecoder.object<CampaignSpecificStep>(
  {
    tag: JsonDecoder.literal('CampaignSpecificStep'),
    contents: JsonDecoder.string(),
  },
  'CampaignSpecificStep',
);

export const epilogueStepDecoder = JsonDecoder.object<EpilogueStep>(
  {
    tag: JsonDecoder.literal('EpilogueStep'),
  },
  'EpilogueStep',
);

export type ScenarioStep = {
  tag: 'ScenarioStep';
  contents: string;
}

export const standaloneScenarioStepDecoder = JsonDecoder.object<StandaloneScenarioStep>(
  {
    tag: JsonDecoder.literal('StandaloneScenarioStep'),
    contents: JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.succeed()], 'contents'),
  },
  'StandabloneScenarioStep',
);

export const scenarioStepDecoder = JsonDecoder.object<ScenarioStep>(
  {
    tag: JsonDecoder.literal('ScenarioStep'),
    contents: JsonDecoder.string()
  },
  'ScenarioStep',
);

export type InterludeStep = {
  tag: 'InterludeStep';
  contents: [number, any];
}

export const interludeStepDecoder = JsonDecoder.object<InterludeStep>(
  {
    tag: JsonDecoder.literal('InterludeStep'),
    contents: JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.succeed()], 'contents')
  },
  'InterludeStep',
);

export type InterludeStepPart = {
  tag: 'InterludeStepPart';
  contents: number;
}

export const interludeStepPartDecoder = JsonDecoder.object<InterludeStepPart>(
  {
    tag: JsonDecoder.literal('InterludeStepPart'),
    contents: JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.succeed(), JsonDecoder.number()], 'contents').map(([contents]) => contents),
  },
  'InterludeStepPart',
);

export type CheckpointStep = {
  tag: 'CheckpointStep';
  contents: number;
}

export const checkpointStepDecoder = JsonDecoder.object<CheckpointStep>(
  {
    tag: JsonDecoder.literal('CheckpointStep'),
    contents: JsonDecoder.number()
  },
  'CheckpointStep',
);

export type UpgradeDeckStep = {
  tag: 'UpgradeDeckStep';
}

export const upgradeStepDecoder = JsonDecoder.object<UpgradeDeckStep>(
  {
    tag: JsonDecoder.literal('UpgradeDeckStep'),
  },
  'UpgradeDeckStep',
);

export const continueCampaignStepDecoder: JsonDecoder.Decoder<ContinueCampaignStep> = JsonDecoder.object<ContinueCampaignStep>(
  {
    tag: JsonDecoder.literal('ContinueCampaignStep'),
    contents: continuationDecoder,
  },
  'ContinueCampaignStep',
);

export const campaignStepDecoder = JsonDecoder.oneOf<CampaignStep>(
  [
    prologueStepDecoder,
    resupplyPointStepDecoder,
    campaignSpecificStepDecoder,
    scenarioStepDecoder,
    standaloneScenarioStepDecoder,
    interludeStepDecoder,
    interludeStepPartDecoder,
    checkpointStepDecoder,
    upgradeStepDecoder,
    epilogueStepDecoder,
    continueCampaignStepDecoder
  ],
  'Question',
);

export function campaignStepName(game: Game, step: CampaignStep) {
  const { t, te } = useI18n();
  if (step.tag === 'ScenarioStep') {
    const scenarioId = step.contents.slice(1)
    const result = scenarios.find((s) => s.id === scenarioId || (s.returnTo && s.returnTo === scenarioId))
    if (result && result.returnTo && result.returnTo === scenarioId) {
      return result.returnToName
    }
    return result?.name || "Unknown Scenario"
  }

  if (step.tag === 'StandaloneScenarioStep') {
    const scenarioId = step.contents[0].slice(1)
    const result = scenarios.find((s) => s.id === scenarioId || (s.returnTo && s.returnTo === scenarioId))
    if (result && result.returnTo && result.returnTo === scenarioId) {
      return result.returnToName
    }
    return result?.name || "Unknown Scenario"
  }

  if (step.tag === 'InterludeStep') {
    if (game.campaign) {
      const key = `${toCamelCase(game.campaign.name)}.interludes.${step.contents[0]}`
      if (te(key)) {
        return t(key)
      }
    }
    return `Interlude ${step.contents[0]}`
  }

  if (step.tag === 'CheckpointStep') {
    return `Checkpoint ${step.contents}`
  }

  if (step.tag === 'ResupplyPoint') {
    return "Resupply Point"
  }

  if (step.tag === 'PrologueStep') {
    if (game.campaign) {
      const key = `${toCamelCase(game.campaign.name)}.headings.prologue`
      if (te(key)) return t(key)
    }
    return t('headings.prologue')
  }

  if (step.tag === 'ContinueCampaignStep') {
    return t('headings.deckCreation')
    // This is a lie, we might need to split this out somehow
  }

  if (step.tag === 'EpilogueStep') {
    return t('headings.epilogue')
  }

  if (step.tag === 'CampaignSpecificStep') {
    if (game.campaign) {
      const key = `${toCamelCase(game.campaign.name)}.headings.${step.contents}`
      console.log(key)
      if (te(key)) return t(key)
    }
  }

  return "Unknown step: " + step.tag
}

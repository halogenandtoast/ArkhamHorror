import * as JsonDecoder from 'ts.data.json'
import scenarios from '@/arkham/data/scenarios'
import type { Game } from '@/arkham/types/Game'
import type { Scenario } from '@/arkham/types/Scenario'
import { toCamelCase } from '@/arkham/helpers'
import { useI18n } from 'vue-i18n';
import { scenarioIdToI18n } from '@/arkham/types/Scenario'
import { ScenarioOptions, defaultScenarioOptions, scenarioOptionsDecoder } from '@/arkham/types/ScenarioOptions';

export type CampaignStep
  = PrologueStep
  | ScenarioStep
  | ScenarioStepWithOptions
  | InterludeStep
  | InterludeStepPart
  | UpgradeDeckStep
  | ChooseDecksStep
  | EpilogueStep
  | ResupplyPoint
  | CheckpointStep
  | CampaignSpecificStep
  | ContinueCampaignStep
  | StandaloneScenarioStep
  | StandaloneScenarioStepWithOptions

export type PrologueStep = {
  tag: 'PrologueStep'
}

export type StandaloneScenarioStep = {
  tag: 'StandaloneScenarioStep'
  contents: [string, any]
}

export type StandaloneScenarioStepWithOptions = {
  tag: 'StandaloneScenarioStepWithOptions'
  contents: [string, any, ScenarioOptions]
}

export type ResupplyPoint = {
  tag: 'ResupplyPoint';
}

export type CampaignSpecificStep = {
  tag: 'CampaignSpecificStep';
  contents: [string, string | null];
}

export type EpilogueStep = {
  tag: 'EpilogueStep';
}

export type Continuation = {
  nextStep: CampaignStep;
  canUpgradeDecks: boolean;
  chooseSideStory: boolean;
}

export type ContinueCampaignStep = {
  tag: 'ContinueCampaignStep';
  contents: Continuation;
}

export const continuationDecoder = JsonDecoder.object<Continuation>(
  {
    nextStep: JsonDecoder.lazy<CampaignStep>(() => campaignStepDecoder),
    canUpgradeDecks: JsonDecoder.boolean(),
    chooseSideStory: JsonDecoder.boolean(),
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
    contents: JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.nullable(JsonDecoder.string())], 'contents'),
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

export const standaloneScenarioStepWithOptionsDecoder = JsonDecoder.object<StandaloneScenarioStepWithOptions>(
  {
    tag: JsonDecoder.literal('StandaloneScenarioStepWithOptions'),
    contents: JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.succeed(), scenarioOptionsDecoder], 'contents'),
  },
  'StandabloneScenarioWithOptionsStep',
);

export const scenarioStepDecoder = JsonDecoder.object<ScenarioStep>(
  {
    tag: JsonDecoder.literal('ScenarioStep'),
    contents: JsonDecoder.string()
  },
  'ScenarioStep',
);

export type ScenarioStepWithOptions = {
  tag: 'ScenarioStepWithOptions';
  contents: [string, ScenarioOptions];
}

export const scenarioStepWithOptionsDecoder = JsonDecoder.object<ScenarioStepWithOptions>(
  {
    tag: JsonDecoder.literal('ScenarioStepWithOptions'),
    contents: JsonDecoder.tuple([JsonDecoder.string(), scenarioOptionsDecoder], 'contents'),
  },
  'ScenarioStepWithOptions',
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

export type ChooseDecksStep = {
  tag: 'ChooseDecksStep';
}

export const chooseDecksStepDecoder = JsonDecoder.object<ChooseDecksStep>(
  {
    tag: JsonDecoder.literal('ChooseDecksStep'),
  },
  'ChooseDecksStep',
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
    scenarioStepWithOptionsDecoder,
    standaloneScenarioStepDecoder,
    standaloneScenarioStepWithOptionsDecoder,
    interludeStepDecoder,
    interludeStepPartDecoder,
    checkpointStepDecoder,
    upgradeStepDecoder,
    chooseDecksStepDecoder,
    epilogueStepDecoder,
    continueCampaignStepDecoder
  ],
  'Question',
);

export function extendWithOptions(step: ScenarioStep | StandaloneScenarioStep, options: Partial<ScenarioOptions>): ScenarioStepWithOptions | StandaloneScenarioStepWithOptions {
  const mergedOptions: ScenarioOptions = {
    ...defaultScenarioOptions,
    ...options,
  }
  if (step.tag === 'StandaloneScenarioStep') {
    return {
      tag: 'StandaloneScenarioStepWithOptions',
      contents: [step.contents[0], step.contents[1], mergedOptions],
    }
  }

  return {
    tag: 'ScenarioStepWithOptions',
    contents: [step.contents, mergedOptions],
  }
}

export function campaignStepName(game: Game, step: CampaignStep, scenario?: Scenario) {
  const { t, te } = useI18n();
  if (scenario) {
    const scenarioId = scenario.id.replace(/^c/, '')
    if (step.tag === 'CheckpointStep') {
      const prefix = scenarioIdToI18n(scenarioId)
      const key = `${prefix}.names.checkpoint${step.contents}`
      if (te(key)) return t(key)
    }
    if (step.tag === 'ScenarioStep') {
      const prefix = scenarioIdToI18n(scenarioId)
      const part = step.contents.slice(-1) === 'b' ? 'part2' : 'part1'
      const key = `${prefix}.names.${part}`
      console.log(key)
      if (te(key)) return t(key)
    }

    const result = scenarios.find((s) => s.id === scenarioId || (s.returnTo && s.returnTo === scenarioId))
    if (result && result.returnTo && result.returnTo === scenarioId) {
      return result.returnToName
    }
    return result?.name || "Unknown Scenario"
  }

  if (step.tag === 'StandaloneScenarioStep') {
    const scenarioId = step.contents[0].slice(1)
    if (step.contents[1]?.tag === 'CheckpointStep') {
      const prefix = scenarioIdToI18n(scenarioId)
      const key = `${prefix}.names.checkpoint${step.contents[1].contents}`
      if (te(key)) return t(key)
    }
    if (step.contents[1]?.tag === 'ScenarioStep') {
      const prefix = scenarioIdToI18n(scenarioId)
      const part = step.contents[1].contents.slice(-1) === 'b' ? 'part2' : 'part1'
      const key = `${prefix}.names.${part}`
      console.log(key)
      if (te(key)) return t(key)
    }
    const result = scenarios.find((s) => s.id === scenarioId || (s.returnTo && s.returnTo === scenarioId))
    if (result && result.returnTo && result.returnTo === scenarioId) {
      return result.returnToName
    }
    return result?.name || "Unknown Scenario"
  }

  if (step.tag === 'ScenarioStep') {
    const scenarioId = step.contents.slice(1)
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
      const [a, b] = step.contents
      const key = `${toCamelCase(game.campaign.name)}.headings.${b ? `${a}.${b}`: a}`
      if (te(key)) return t(key)
    }
  }

  return "Unknown step: " + step.tag
}

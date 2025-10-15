import * as JsonDecoder from 'ts.data.json';
import { Source, sourceDecoder } from '@/arkham/types/Source';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { Cost, costDecoder } from '@/arkham/types/Cost';
import { Action, actionDecoder } from '@/arkham/types/Action';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';

export type ServitorAbility = {
  tag: "ServitorAbility"
  action: Action
}

export const servitorAbilityDecoder = JsonDecoder.object<ServitorAbility>({
  tag: JsonDecoder.literal("ServitorAbility"),
  action: actionDecoder
}, 'ServitorAbility')

export type FastAbility = {
  tag: "FastAbility"
  cost: Cost
}

export const fastAbilityDecoder = JsonDecoder.object<FastAbility>({
  tag: JsonDecoder.literal("FastAbility'").map(() => "FastAbility"),
  cost: costDecoder
}, 'FastAbility')

export type ReactionAbility = {
  tag: "ReactionAbility"
  cost: Cost
  // window :: WindowMatcher
}

export const reactionAbilityDecoder = JsonDecoder.object<ReactionAbility>({
  tag: JsonDecoder.literal("ReactionAbility"),
  cost: costDecoder
}, 'ReactionAbility')

export type CustomizationReaction = {
  tag: "CustomizationReaction"
  label: string
  cost: Cost
  // window :: WindowMatcher
}

export const customizationReactionDecoder = JsonDecoder.object<CustomizationReaction>({
  tag: JsonDecoder.literal("CustomizationReaction"),
  label: JsonDecoder.string(),
  cost: costDecoder
}, 'CustomizationReaction')

export type ConstantReaction = {
  tag: "ConstantReaction"
  label: string
  cost: Cost
  // window :: WindowMatcher
}

export const constantReactionDecoder = JsonDecoder.object<ConstantReaction>({
  tag: JsonDecoder.literal("ConstantReaction"),
  label: JsonDecoder.string(),
  cost: costDecoder
}, 'ConstantReaction')

export type ActionAbility = {
  tag: "ActionAbility"
  actions: Action[]
  cost: Cost
}

export const actionAbilityDecoder = JsonDecoder.object<ActionAbility>({
  tag: JsonDecoder.literal("ActionAbility"),
  actions: JsonDecoder.array(actionDecoder, 'Action[]'),
  cost: costDecoder,
}, 'ActionAbility')

export type ActionAbilityWithSkill = {
  tag: "ActionAbilityWithSkill"
  actions: Action[]
  skillType: SkillType
  cost: Cost
}

export const actionAbilityWithSkillDecoder = JsonDecoder.object<ActionAbilityWithSkill>({
  tag: JsonDecoder.literal("ActionAbilityWithSkill"),
  actions: JsonDecoder.array(actionDecoder, 'Actions[]'),
  skillType: skillTypeDecoder,
  cost: costDecoder
}, 'ActionAbility')

export type ActionAbilityWithBefore = {
  tag: "ActionAbilityWithBefore"
  actions: Action[]
  actionBefore: Action
  cost: Cost
}

export const actionAbilityWithBeforeDecoder = JsonDecoder.object<ActionAbilityWithBefore>({
  tag: JsonDecoder.literal("ActionAbilityWithBefore"),
  actions: JsonDecoder.array(actionDecoder, 'Action[]'),
  actionBefore: actionDecoder,
  cost: costDecoder
}, 'ActionAbility')

export type SilentForcedAbility = {
  tag: "SilentForcedAbility"
}

export const silentForcedAbilityDecoder = JsonDecoder.object<SilentForcedAbility>({
  tag: JsonDecoder.literal("SilentForcedAbility"),
}, 'SilentForcedAbility')

export type ForcedAbility = {
  tag: "ForcedAbility"
}

export const forcedAbilityDecoder = JsonDecoder.object<ForcedAbility>({
  tag: JsonDecoder.literal("ForcedAbility"),
}, 'ForcedAbility')

export type ForcedAbilityWithCost = {
  tag: "ForcedAbilityWithCost"
  cost: Cost
}

export const forcedAbilityWithCostDecoder = JsonDecoder.object<ForcedAbilityWithCost>({
  tag: JsonDecoder.literal("ForcedAbilityWithCost"),
  cost: costDecoder
}, 'ForcedAbilityWithCost')

export type AbilityEffect = {
  tag: "AbilityEffect"
  cost: Cost
}

export const abilityEffectDecoder = JsonDecoder.object<AbilityEffect>({
  tag: JsonDecoder.literal("AbilityEffect"),
  cost: costDecoder
}, 'AbilityEffect')

export type DelayedAbility = {
  tag: "DelayedAbility"
  abilityType: AbilityType
}

export const delayedAbilityDecoder = JsonDecoder.object<DelayedAbility>({
  tag: JsonDecoder.literal("DelayedAbility"),
  abilityType: JsonDecoder.lazy<AbilityType>(() => abilityTypeDecoder)
}, 'DelayedAbility')

export type Objective = {
  tag: "Objective"
  abilityType: AbilityType
}

export const objectiveDecoder = JsonDecoder.object<Objective>({
  tag: JsonDecoder.literal("Objective"),
  abilityType: JsonDecoder.lazy<AbilityType>(() => abilityTypeDecoder)
}, 'Objective')

export type Haunted = {
  tag: "Haunted"
}

export const hauntedDecoder = JsonDecoder.object<Haunted>({
  tag: JsonDecoder.literal("Haunted"),
}, 'Haunted')


export type AbilityType = ServitorAbility | FastAbility | ReactionAbility | CustomizationReaction | ConstantReaction | ActionAbility | ActionAbilityWithSkill | ActionAbilityWithBefore | SilentForcedAbility | ForcedAbility | ForcedAbilityWithCost | AbilityEffect | Objective | Haunted | DelayedAbility

export type ForcedWhen = {
  tag: "ForcedWhen"
  abilityType: AbilityType
}

export const forcedWhenDecoder = JsonDecoder.object<ForcedWhen>({
  tag: JsonDecoder.literal("ForcedWhen"),
  abilityType: JsonDecoder.lazy(() => abilityTypeDecoder)
}, 'Haunted')


export const abilityTypeDecoder: JsonDecoder.Decoder<AbilityType> = JsonDecoder.oneOf<AbilityType>([
  servitorAbilityDecoder,
  fastAbilityDecoder,
  reactionAbilityDecoder,
  customizationReactionDecoder,
  constantReactionDecoder,
  actionAbilityDecoder,
  actionAbilityWithSkillDecoder,
  actionAbilityWithBeforeDecoder,
  silentForcedAbilityDecoder,
  forcedAbilityDecoder,
  forcedAbilityWithCostDecoder,
  abilityEffectDecoder,
  objectiveDecoder,
  delayedAbilityDecoder,
  hauntedDecoder,
  forcedWhenDecoder.map((fw: ForcedWhen) => fw.abilityType)
], 'AbilityType')

export type DisplayAs = 'DisplayAsAction' | 'DisplayAsCard'

export type Ability = {
  type: AbilityType
  source: Source
  tooltip: string | null
  displayAs: DisplayAs | null
  index: number
  target: Target | null
}

const displayAsDecoder = JsonDecoder.oneOf<DisplayAs>([
  JsonDecoder.literal('DisplayAsAction'),
  JsonDecoder.literal('DisplayAsCard')
], 'DisplayAs')

export const abilityDecoder = JsonDecoder.object<Ability>(
  {
    type: abilityTypeDecoder,
    source: sourceDecoder,
    tooltip: JsonDecoder.nullable(JsonDecoder.string()),
    displayAs: JsonDecoder.nullable(displayAsDecoder),
    index: JsonDecoder.number(),
    target: JsonDecoder.nullable(targetDecoder)
  }, 'Ability')

export type AbilityRef = {
  source: Source
  index: number
}

export const abilityRefDecoder = JsonDecoder.object<AbilityRef>(
  {
    source: sourceDecoder,
    index: JsonDecoder.number()
  }, 'AbilityRef')
